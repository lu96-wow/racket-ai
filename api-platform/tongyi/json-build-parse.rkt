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
 build-messages build-user-message
 build-assistant-message build-tool-result
 build-tool
 build-chat-request json->string string->json
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

(define (build-message
         #:role [role "user"]
         #:content [content ""]
         #:tool_calls [tcs #f]
         #:tool_call_id [tcid #f]
         #:name [name #f])
  (let* ([h  (hasheq 'role role 'content content)]
         [h  (if tcs  (hash-set h 'tool_calls tcs)    h)]
         [h  (if tcid (hash-set h 'tool_call_id tcid) h)]
         [h  (if name (hash-set h 'name name)         h)])
    h))

(define (build-messages . msgs) msgs)

(define (build-user-message #:content content)
  (build-message #:role "user" #:content content))

(define (build-assistant-message #:content content
                                 #:tool_calls [tcs #f])
  (build-message #:role "assistant" #:content content #:tool_calls tcs))

(define (build-tool-result #:tool_call_id tool_call_id #:content content)
  (build-message #:role "tool" #:content content #:tool_call_id tool_call_id))

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

(define (build-chat-request #:model model #:messages messages
                            #:stream [stream #f]
                            #:max_tokens [max_tokens #f]
                            #:temperature [temp #f]
                            #:top_p [top_p #f]
                            #:n [n #f]
                            #:tools [tools #f]
                            #:stop [stop #f])
  (define (maybe h k v) (if v (hash-set h k v) h))
  (maybe (maybe (maybe (maybe (maybe
                               (maybe (maybe (hasheq 'model model 'messages messages)
                                             'stream stream) 'max_tokens max_tokens)
                               'temperature temp) 'top_p top_p)
                     'n n) 'tools tools)
         'stop stop))

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
