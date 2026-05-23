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
 tongyi-chat
 tongyi-chat/stream)

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
;; tongyi-chat/stream : hasheq? #:stop? stop? handler ... -> void
;; 流式调用，回调分发 content / tool-calls，支持 #:stop? 中断
;; ============================================================

(define (tongyi-chat/stream request-hash
                            #:stop? stop?
                            . handlers)
  (define body-str (json->string request-hash))
  (define-values (status resp-headers body-port)
    (http-post (chat-url) (chat-headers) body-str))
  (check-http-error! status resp-headers body-port)

  (define dispatch
    (for/hash ([h (in-list handlers)])
      (match h
        [(cons type proc) (values type proc)])))

  (for ([data (in-sse body-port #:stop? stop?)])
    (define delta (choice-delta (response-first-choice data)))
    (cond
      [(delta-content delta)
       => (lambda (c) (define h (hash-ref dispatch 'content #f)) (when h (h c)))]
      [(delta-tool-calls delta)
       => (lambda (tcs) (define h (hash-ref dispatch 'tool-calls #f)) (when h (h tcs)))]
      [else (void)]))
  (void))