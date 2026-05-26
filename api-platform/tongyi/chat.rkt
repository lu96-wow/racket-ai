#lang racket
;; ============================================================
;; api-platform/tongyi/chat.rkt — Tongyi/Qwen 对话接口
;;
;; 提供与 deepseek/chat.rkt 一致的导出接口，可互换。
;; ============================================================

(require "../../api-config/tongyi.rkt"
         "json-build-parse.rkt"
         "../../net-io.rkt")

(provide
 tongyi-chat            ; request-hash -> response-jsexpr  (非流式)
 tongyi-chat/stream     ; request-hash handler ... -> response-jsexpr  (流式, 回调+累积返回)
 )

;; ============================================================
;; 内部: 构建 URL、Headers、检查 HTTP 错误
;; ============================================================

(define (chat-url)
  (string-append tongyi-base-url tongyi-chat-endpoint))

(define (chat-headers)
  (list (format "Authorization: Bearer ~a" (tongyi-api-key))
        "Content-Type: application/json"))

;; HTTP 错误码 → 中文说明
(define http-error-messages
  (hash 400 "请求体格式错误，请根据错误信息修改请求体"
        401 "API key 错误，认证失败，请检查 API key"
        402 "账号余额不足，请确认账户余额并充值"
        422 "请求体参数错误，请根据错误信息修改相关参数"
        429 "请求速率达到上限，请合理规划请求速率"
        500 "服务器内部故障，请等待后重试"
        503 "服务器负载过高，请稍后重试"))

(define (parse-status-code status-bytes)
  (define status-str (bytes->string/utf-8 status-bytes))
  (define parts (regexp-split #rx" " status-str))
  (if (>= (length parts) 2)
      (string->number (list-ref parts 1))
      #f))

(define (check-http-error! status headers body-port)
  (when (http-error? status headers body-port)
    (define body-text (read-response-body body-port))
    (define code (parse-status-code status))
    (define desc (and code (hash-ref http-error-messages code #f)))
    (define msg (if desc
                    (format "Tongyi ~a: ~a" code desc)
                    (format "Tongyi ~a" (bytes->string/utf-8 status))))
    (raise (make-exn:fail
            (string-append msg "\n" body-text)
            (current-continuation-marks)))))

;; ============================================================
;; tongyi-chat : hasheq? -> jsexpr?
;; 非流式调用
;; ============================================================

(define (tongyi-chat request-hash)
  (define body-str (json->string request-hash))
  (define-values (status resp-headers body-port)
    (http-post (chat-url) (chat-headers) body-str))
  (check-http-error! status resp-headers body-port)
  (string->json (read-response-body body-port)))

;; ============================================================
;; tongyi-chat/stream : hasheq? #:stop? (-> boolean) (cons/c symbol? procedure?) ...
;;                     -> jsexpr?
;;
;; 流式调用 + 全局累积返回。
;;
;; 两种输出通道：
;;   1. 回调 —— 实时回调 content / tool-calls 分片
;;   2. 返回值 —— 流结束后返回完整 response（结构与 tongyi-chat 一致）
;;
;; #:stop? 中断时返回截断的累积 response。
;; ============================================================

(define (tongyi-chat/stream request-hash
                            #:stop? [stop? (lambda () #f)]
                            . handlers)
  (define body-str (json->string request-hash))
  (define-values (status resp-headers body-port)
    (http-post (chat-url) (chat-headers) body-str))
  (check-http-error! status resp-headers body-port)

  (define dispatch
    (for/hash ([h (in-list handlers)])
      (match h
        [(cons type proc) (values type proc)])))

  ;; ---------- 累加器 ----------
  (define acc-content    "")
  (define acc-tool-calls (hash))         ;; index → 合并后的 tool-call hash
  (define last-data      #f)

  ;; 工具分片合并（同 tool.rkt 的 deep-merge）
  (define (deep-merge b o)
    (for/fold ([m b]) ([(k v) (in-hash o)])
      (cond
        [(and (hash? v) (hash-has-key? m k) (hash? (hash-ref m k)))
         (hash-set m k (deep-merge (hash-ref m k) v))]
        [(and (string? v) (hash-has-key? m k) (string? (hash-ref m k)))
         (hash-set m k (string-append (hash-ref m k) v))]
        [else (hash-set m k v)])))

  ;; ---------- 流式循环：回调 + 累积 ----------
  (for ([data (in-sse body-port #:stop? stop?)])
    (set! last-data data)
    (define delta (choice-delta (response-first-choice data)))

    ;; content 分片
    (let ([c (delta-content delta)])
      (when c
        (set! acc-content (string-append acc-content c))
        (let ([h (hash-ref dispatch 'content #f)])
          (when h (h c)))))

    ;; tool-calls 分片（按 index 合并）
    (let ([tcs (delta-tool-calls delta)])
      (when tcs
        (for ([tc (in-list tcs)])
          (define idx (hash-ref tc 'index 0))
          (set! acc-tool-calls
                (hash-set acc-tool-calls idx
                          (deep-merge (hash-ref acc-tool-calls idx (hasheq)) tc))))
        (let ([h (hash-ref dispatch 'tool-calls #f)])
          (when h (h tcs))))))

  ;; ---------- 构建返回 response ----------
  (define resp-id      (and last-data (hash-ref last-data 'id #f)))
  (define resp-model   (and last-data (hash-ref last-data 'model #f)))
  (define resp-created (and last-data (hash-ref last-data 'created #f)))
  (define resp-usage   (and last-data (hash-ref last-data 'usage #f)))

  (define raw-finish
    (and last-data
         (let ([c (response-first-choice last-data)])
           (hash-ref c 'finish_reason #f))))
  (define finish-reason (if (eq? raw-finish 'null) #f raw-finish))

  (define msg
    (let ([h (hasheq 'role "assistant" 'content acc-content)])
      (let ([tcl (for/list ([(k v) (in-hash acc-tool-calls)]) v)])
        (if (pair? tcl)
            (hash-set h 'tool_calls tcl)
            h))))

  (define choice
    (let ([h (hasheq 'index 0 'message msg)])
      (if finish-reason
          (hash-set h 'finish_reason finish-reason)
          h)))

  (hasheq 'id (or resp-id "")
          'object "chat.completion"
          'model (or resp-model "")
          'created (or resp-created 0)
          'choices (list choice)
          'usage (or resp-usage (hasheq))))