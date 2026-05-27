#lang racket

;; ============================================================
;; api-platform/deepseek/json-build-parse.rkt
;; DeepSeek API 专用 JSON 构建/解析原语
;; 底层直接用 Racket 原生 json 库 (hasheq + jsexpr->string)
;;
;; jsexpr 规则:
;;   key    — symbol 或 string (hasheq 的 key)
;;   value  — string | number | boolean | 'null | list | hasheq
;;   数组   — list (不是 vector!)
;; https://api-docs.deepseek.com/
;; ============================================================

(require json)

(provide
 build-messages build-message build-user-message
 build-assistant-message build-tool-result
 build-tool
 build-thinking
 build-chat-request
 chat-request-keys message-keys
 json->string string->json
 response-id response-model response-usage response-first-choice
 response-content response-tool-calls response-reasoning
 choice-delta choice-message
 delta-content delta-reasoning delta-tool-calls
 tool-call-id tool-call-func-name tool-call-func-args)

;; ============================================================
;; 别名
;; ============================================================

(define json->string jsexpr->string)
(define string->json  string->jsexpr)

;; ============================================================
;; 消息构建
;; ============================================================

;; 可用参数键列表 (用于 build-message)
(define message-keys
  '(role content reasoning_content tool_calls tool_call_id name))

(define (build-message msg)
  (define base (hasheq 'role (hash-ref msg 'role "user")
                        'content (hash-ref msg 'content "")))
  (for/fold ([h base]) ([k (in-list '(reasoning_content tool_calls tool_call_id name))])
    (define v (hash-ref msg k #f))
    (if v (hash-set h k v) h)))

(define (build-messages . msgs)
  msgs)

(define (build-user-message #:content content)
  (build-message (hasheq 'role "user" 'content content)))

(define (build-assistant-message #:content content
                                 #:reasoning_content [rc #f]
                                 #:tool_calls [tcs #f])
  (define h (hasheq 'role "assistant" 'content content))
  (define h2 (if rc (hash-set h 'reasoning_content rc) h))
  (define h3 (if tcs (hash-set h2 'tool_calls tcs) h2))
  (build-message h3))

(define (build-tool-result #:tool_call_id tool_call_id #:content content)
  (build-message (hasheq 'role "tool" 'content content 'tool_call_id tool_call_id)))

;; ============================================================
;; 工具构建
;; ============================================================

(define (build-tool #:name name #:description description #:parameters parameters)
  (hasheq 'type "function"
          'function (hasheq 'name name
                            'description description
                            'parameters parameters)))

(define (build-thinking #:enabled? enabled?)
  (hasheq 'type (if enabled? "enabled" "disabled")))

;; ============================================================
;; 请求构建
;; ============================================================

;; 可用参数键列表 (用于 build-chat-request)
(define chat-request-keys
  '(model messages stream max_tokens temperature top_p
    n thinking reasoning_effort tools stop))

(define (build-chat-request params)
  (define base (hasheq 'model (hash-ref params 'model)
                        'messages (hash-ref params 'messages)))
  ;; thinking: #t → auto-build, #f → omit, hash → use as-is
  (define with-thinking
    (match (hash-ref params 'thinking #f)
      [#t     (hash-set base 'thinking (build-thinking #:enabled? #t))]
      [#f     base]
      [thk    (hash-set base 'thinking thk)]))
  (for/fold ([h with-thinking])
            ([k (in-list '(stream max_tokens temperature top_p n
                          reasoning_effort tools stop))])
    (define v (hash-ref params k #f))
    (if v (hash-set h k v) h)))

;; ============================================================
;; 响应解析
;; ============================================================

(define (response-id resp)              (hash-ref resp 'id))
(define (response-model resp)           (hash-ref resp 'model))
(define (response-choices resp)         (hash-ref resp 'choices))
(define (response-usage resp)           (hash-ref resp 'usage #f))
(define (response-first-choice resp)    (list-ref (response-choices resp) 0))
(define (response-content resp)
  (message-content (choice-message (response-first-choice resp))))

(define (response-reasoning resp)
  (message-reasoning (choice-message (response-first-choice resp))))

(define (response-tool-calls resp)
  (message-tool-calls (choice-message (response-first-choice resp))))

(define (choice-message c)       (hash-ref c 'message #f))
(define (choice-delta c)         (hash-ref c 'delta #f))
;; null->#f : JSON null 解析为 symbol 'null，可选字段应转为 #f
(define (null->#f v) (if (eq? v 'null) #f v))

(define (message-content m)    (null->#f (hash-ref m 'content #f)))
(define (message-reasoning m)  (null->#f (hash-ref m 'reasoning_content #f)))
(define (message-tool-calls m) (null->#f (hash-ref m 'tool_calls #f)))

(define delta-content    message-content)
(define delta-reasoning  message-reasoning)
(define delta-tool-calls message-tool-calls)

(define (tool-call-id tc)        (hash-ref tc 'id))
(define (tool-call-func-name tc) (hash-ref (hash-ref tc 'function) 'name))
(define (tool-call-func-args tc) (hash-ref (hash-ref tc 'function) 'arguments))