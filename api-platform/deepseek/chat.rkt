#lang racket

;; ============================================================
;; api-platform/deepseek/chat.rkt — DeepSeek 平台对话接口
;;
;; 对接:
;;   api-config/deepseek.rkt           — URL、模型、认证
;;   json-build-parse.rkt              — 请求构建/响应解析
;;   net-io.rkt                        — HTTP 传输 + SSE 读取
;; ============================================================

(require "../../api-config/deepseek.rkt"
         "json-build-parse.rkt"
         "../../net-io.rkt")

(provide
 ;; ---- 便捷调用 ----
 deepseek-chat            ; request-hash -> response-jsexpr  (非流式)
 deepseek-chat/stream     ; request-hash handler ... -> void  (流式, 回调分发)
 )

;; ============================================================
;; 内部: 构建 URL、Headers、检查 HTTP 错误
;; ============================================================

(define (chat-url)
  (string-append deepseek-base-url deepseek-chat-endpoint))

(define (chat-headers)
  (list (format "Authorization: Bearer ~a" (deepseek-api-key))
        "Content-Type: application/json"))

;; DeepSeek HTTP 错误码 → 中文说明 https://api-docs.deepseek.com/quick_start/error_codes
(define http-error-messages
  (hash 400 "请求体格式错误，请根据错误信息修改请求体"
        401 "API key 错误，认证失败，请检查 API key"
        402 "账号余额不足，请确认账户余额并充值"
        422 "请求体参数错误，请根据错误信息修改相关参数"
        429 "请求速率达到上限，请合理规划请求速率"
        500 "服务器内部故障，请等待后重试"
        503 "服务器负载过高，请稍后重试"))

;; 从 "HTTP/1.1 400 Bad Request" 中提取状态码
(define (parse-status-code status-bytes)
  (define status-str (bytes->string/utf-8 status-bytes))
  (define parts (regexp-split #rx" " status-str))
  (if (>= (length parts) 2)
      (string->number (list-ref parts 1))
      #f))

;; 检查 HTTP 响应状态，出错则抛异常（含中文错误说明）
(define (check-http-error! status headers body-port)
  (when (http-error? status headers body-port)
    (define body-text (read-response-body body-port))
    (define code (parse-status-code status))
    (define desc (and code (hash-ref http-error-messages code #f)))
    (define msg (if desc
                    (format "DeepSeek ~a: ~a" code desc)
                    (format "DeepSeek ~a" (bytes->string/utf-8 status))))
    (raise (make-exn:fail
            (string-append msg "\n" body-text)
            (current-continuation-marks)))))

;; ============================================================
;; deepseek-chat : hasheq? -> jsexpr?
;;
;; 非流式调用。自动序列化、发送、解析。
;; 用法:
;;   (define resp (deepseek-chat
;;                  (build-chat-request deepseek-v4-pro
;;                    (list (build-user-message "hi"))
;;                    #:thinking (build-thinking #t)))))
;;   (response-content resp)
;; ============================================================

(define (deepseek-chat request-hash)
  (define body-str (json->string request-hash))
  (define-values (status resp-headers body-port)
    (http-post (chat-url) (chat-headers) body-str))
  (check-http-error! status resp-headers body-port)
  (string->json (read-response-body body-port)))

;; ============================================================
;; deepseek-chat/stream : hasheq? (cons/c symbol? procedure?) ... -> void
;;
;; 流式调用。接受 (cons 事件类型 回调函数) 对，按事件分发。
;; content / reasoning / tool-calls 用回调（流中多次发生），
;; done 是自然结束（函数返回后继续执行），error 抛异常。
;;
;; 用法:
;;   (define buf "")
;;   (deepseek-chat/stream req
;;     (cons 'content (λ (c) (set! buf (string-append buf c)) (display c))))
;;   (printf "\n完成: ~a\n" buf)
;; ============================================================

(define (deepseek-chat/stream request-hash
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

  (for ([data (in-sse body-port #:stop? stop?)])
    (define delta (choice-delta (response-first-choice data)))
    (cond
      [(delta-content delta)
       => (λ (c) (define h (hash-ref dispatch 'content #f)) (when h (h c)))]
      [(delta-reasoning delta)
       => (λ (r) (define h (hash-ref dispatch 'reasoning #f)) (when h (h r)))]
      [(delta-tool-calls delta)
       => (λ (tcs) (define h (hash-ref dispatch 'tool-calls #f)) (when h (h tcs)))]
      [else (void)]))
  (void))