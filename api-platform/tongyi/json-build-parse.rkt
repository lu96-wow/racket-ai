#lang racket
;; ============================================================
;; api-platform/tongyi/json-build-parse.rkt
;; Tongyi/Qwen API JSON build/parse primitives
;;
;; Provide list matches deepseek version exactly for drop-in replacement.
;; Format is OpenAI-compatible.
;; ============================================================

(require json)

(provide
 build-messages build-message build-user-message
 build-assistant-message build-tool-result
 build-tool
 build-chat-request
 chat-request-keys message-keys
 json->string string->json
 response-id response-model response-usage response-first-choice
 response-content response-tool-calls
 choice-delta
 delta-content delta-tool-calls
 tool-call-id tool-call-func-name tool-call-func-args)

(define json->string jsexpr->string)
(define string->json  string->jsexpr)

;; ============================================================
;; Message building
;; ============================================================

;; Available parameter keys (for build-message)
(define message-keys
  '(role content tool_calls tool_call_id name))

(define (build-message msg)
  (define base (hasheq 'role (hash-ref msg 'role "user")
                        'content (hash-ref msg 'content "")))
  (for/fold ([h base]) ([k (in-list '(tool_calls tool_call_id name))])
    (define v (hash-ref msg k #f))
    (if v (hash-set h k v) h)))

(define (build-messages . msgs) msgs)

(define (build-user-message #:content content)
  (build-message (hasheq 'role "user" 'content content)))

(define (build-assistant-message #:content content
                                 #:tool_calls [tcs #f])
  (define h (hasheq 'role "assistant" 'content content))
  (define h2 (if tcs (hash-set h 'tool_calls tcs) h))
  (build-message h2))

(define (build-tool-result #:tool_call_id tool_call_id #:content content)
  (build-message (hasheq 'role "tool" 'content content 'tool_call_id tool_call_id)))

;; ============================================================
;; Tool building
;; ============================================================

(define (build-tool #:name name #:description description #:parameters parameters)
  (hasheq 'type "function"
          'function (hasheq 'name name
                            'description description
                            'parameters parameters)))

;; ============================================================
;; Request building
;; ============================================================

;; Available parameter keys (for build-chat-request)
(define chat-request-keys
  '(model messages stream max_tokens temperature top_p
    n tools stop))

(define (build-chat-request params)
  (define base (hasheq 'model (hash-ref params 'model)
                        'messages (hash-ref params 'messages)))
  (for/fold ([h base])
            ([k (in-list '(stream max_tokens temperature top_p n tools stop))])
    (define v (hash-ref params k #f))
    (if v (hash-set h k v) h)))

;; ============================================================
;; Response parsing
;; ============================================================

(define (response-id resp)              (hash-ref resp 'id))
(define (response-model resp)           (hash-ref resp 'model))
(define (response-choices resp)         (hash-ref resp 'choices))
(define (response-usage resp)           (hash-ref resp 'usage #f))
(define (response-first-choice resp)    (list-ref (response-choices resp) 0))

(define (response-content resp)
  (message-content (choice-message (response-first-choice resp))))

(define (response-tool-calls resp)
  (message-tool-calls (choice-message (response-first-choice resp))))

(define (choice-message c)       (hash-ref c 'message #f))
(define (choice-delta c)         (hash-ref c 'delta #f))

(define (null->#f v) (if (eq? v 'null) #f v))

(define (message-content m)    (null->#f (hash-ref m 'content #f)))
(define (message-tool-calls m) (null->#f (hash-ref m 'tool_calls #f)))

(define delta-content    message-content)
(define delta-tool-calls message-tool-calls)

(define (tool-call-id tc)        (hash-ref tc 'id))
(define (tool-call-func-name tc) (hash-ref (hash-ref tc 'function) 'name))
(define (tool-call-func-args tc) (hash-ref (hash-ref tc 'function) 'arguments))
