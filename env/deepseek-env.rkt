#lang racket

;; ============================================================
;; env/deepseek-env.rkt -- DeepSeek AI Dui Hua Huan Jing
;;
;; Jiang model, mo ren can shu, gong ju he liu shi hui tiao
;; da bao wei "huan jing", jian shao DeepSeek API diao yong
;; de yang ban dai ma.
;;
;; Yong Fa:
;;   (define my-env
;;     (make-env #:model deepseek-v4-flash
;;               #:max_tokens 4096
;;               #:tools default-tools
;;               #:on-content (lambda (c) (display c) (flush-output))))
;;
;;   (define resp (env-chat my-env "Yong yi ju hua jie shao ni zi ji."))
;;   (env-chat/stream my-env "Cong 1 shu dao 5")
;;   (env-chat (env-set my-env #:temperature 0.9) "Jiang ge xiao hua")
;;
;; Gong ju xun huan (tool call loop) you wai bu zi xing chu li.
;; ============================================================

(require "../format-color/core.rkt"
         "../format-color/styles.rkt"
         "../api-platform/deepseek/chat.rkt"
         "../api-platform/deepseek/json-build-parse.rkt"
         "../tools/tool.rkt")

(provide
 ;; huan jing API
 make-env env-set env?
 env-model env-params env-tools env-callbacks env-stop?
 env-chat env-chat/stream
 env-print

 ;; xiang ying jie xi (re-export)
 ;; xiao xi gou jian (re-export)
 build-messages build-user-message build-assistant-message build-tool-result

 ;; xiang ying jie xi (re-export)
 response-id response-model response-usage
 response-content response-tool-calls response-reasoning
 response-first-choice
 tool-call-id tool-call-func-name tool-call-func-args
 tool-chat-request tool-stop-chat-request tool-cancel-chat-request)

;; ============================================================
;; Shao Bing Zhi: qu fen "wei ti gong" he "#f shi he fa zhi"
;; ============================================================

(define-syntax-rule (omit) (box 'omit))
(define (omit? v) (and (box? v) (eq? (unbox v) 'omit)))

;; ============================================================
;; Environment Jie Gou Ti
;; ============================================================

(struct env (model           ; string
             params          ; hasheq
             tools           ; tools? | #f
             callbacks       ; hasheq
             stop?)          ; (-> boolean)
  #:transparent)

;; ============================================================
;; Nei Bu: Gui Fan Hua Xiao Xi Can Shu
;; ============================================================

(define (normalize-messages msg)
  (cond
    [(string? msg)
     (build-messages (build-user-message #:content msg))]
    [(and (list? msg) (pair? msg))
     msg]
    [else
     (error 'env-chat
            "expected string or list of messages, got ~a"
            msg)]))

;; ============================================================
;; Nei Bu: Gou Jian Qing Qiu Ha Xi
;; He Bing: env mo ren can shu + xiao xi + lin shi fu gai
;; ============================================================

(define (build-req-hash env messages
                        #:stream [stream? #f]
                        #:override [override #f])
  (define model (or (env-model env)
                    (error 'env-chat "model not set in environment")))
  (define h (hasheq 'model model 'messages messages))
  (define h2 (if stream? (hash-set h 'stream #t) h))
  (define h3
    (if (env-tools env)
        (hash-set h2 'tools (tools-schemas (env-tools env)))
        h2))
  (define h4
    (for/fold ([h h3]) ([(k v) (in-hash (env-params env))])
      (hash-set h k v)))
  (if override
      (for/fold ([h h4]) ([(k v) (in-hash override)])
        (hash-set h k v))
      h4))

;; ============================================================
;; make-env : Chuang Jian DeepSeek Huan Jing
;;
;; Suo you #: can shu jun wei ke xuan.
;; #:model zai shi yong qian bi xu she zhi.
;;
;; DeepSeek zhuan you xuan xiang:
;;   #:thinking -- boolean huo hash, qi dong si kao mo shi
;;   #:reasoning-effort -- "low" | "medium" | "high"
;; ============================================================

(define (make-env
         #:model [model #f]
         #:max_tokens [max-tokens (omit)]
         #:temperature [temperature (omit)]
         #:top-p [top-p (omit)]
         #:thinking [thinking (omit)]
         #:reasoning-effort [reasoning-effort (omit)]
         #:n [n (omit)]
         #:stop [stop (omit)]
         #:stream [stream (omit)]
         #:tools [tools #f]
         #:on-content [on-content #f]
         #:on-reasoning [on-reasoning #f]
         #:on-tool-calls [on-tool-calls #f]
         #:stop? [stop? (lambda () #f)])
  (define params
    (for/hash ([(k v) (in-hash
                       (hasheq 'max_tokens max-tokens
                               'temperature temperature
                               'top_p top-p
                               'thinking thinking
                               'reasoning_effort reasoning-effort
                               'n n
                               'stop stop
                               'stream stream))]
               #:when (and (not (omit? v)) v))
      (values k v)))
  (define callbacks
    (for/hash ([(k v) (in-hash
                       (hasheq 'content on-content
                               'reasoning on-reasoning
                               'tool-calls on-tool-calls))]
               #:when v)
      (values k v)))
  (env model params tools callbacks stop?))

;; ============================================================
;; env-set : Chuang Jian Xiu Gai Hou De Huan Jing Fu Ben
;;
;; Yong Fa:
;;   (env-set my-env #:max_tokens 512 #:temperature 0.9)
;;   (env-set my-env #:on-content #f)   ;; yi chu gai hui diao
;; ============================================================

(define (env-set e
                 #:model [model (omit)]
                 #:max_tokens [max-tokens (omit)]
                 #:temperature [temperature (omit)]
                 #:top-p [top-p (omit)]
                 #:thinking [thinking (omit)]
                 #:reasoning-effort [reasoning-effort (omit)]
                 #:n [n (omit)]
                 #:stop [stop (omit)]
                 #:stream [stream (omit)]
                 #:tools [tools (omit)]
                 #:on-content [on-content (omit)]
                 #:on-reasoning [on-reasoning (omit)]
                 #:on-tool-calls [on-tool-calls (omit)]
                 #:stop? [stop? (omit)])
  (define new-model
    (if (omit? model) (env-model e) model))
  (define new-tools
    (if (omit? tools) (env-tools e) tools))
  (define new-stop?
    (if (omit? stop?) (env-stop? e) stop?))

  ;; params: xin zhi fu gai jiu zhi, #f shan chu gai can shu
  (define new-params
    (let ([base (env-params e)])
      (for/fold ([h base])
                ([(k v) (in-hash
                         (hasheq 'max_tokens max-tokens
                                 'temperature temperature
                                 'top_p top-p
                                 'thinking thinking
                                 'reasoning_effort reasoning-effort
                                 'n n
                                 'stop stop
                                 'stream stream))]
                 #:when (not (omit? v)))
        (if v (hash-set h k v) (hash-remove h k)))))

  ;; callbacks: xin zhi fu gai jiu zhi, #f shan chu gai hui diao
  (define new-callbacks
    (let ([base (env-callbacks e)])
      (for/fold ([h base])
                ([(k v) (in-hash
                         (hasheq 'content on-content
                                 'reasoning on-reasoning
                                 'tool-calls on-tool-calls))]
                 #:when (not (omit? v)))
        (if v (hash-set h k v) (hash-remove h k)))))

  (env new-model new-params
       new-tools new-callbacks new-stop?))

;; ============================================================
;; env-chat : Fei Liu Shi Dui Hua
;; ============================================================

(define (env-chat env msg-like #:override [override #f])
  (define messages (normalize-messages msg-like))
  (define req-hash (build-req-hash env messages
                                   #:stream #f
                                   #:override override))
  (define req (build-chat-request req-hash))
  (deepseek-chat req))

;; ============================================================
;; env-chat/stream : Liu Shi Dui Hua
;; ============================================================

(define (env-chat/stream env msg-like
                         #:on-content [on-content (omit)]
                         #:on-reasoning [on-reasoning (omit)]
                         #:on-tool-calls [on-tool-calls (omit)]
                         #:stop? [stop? (omit)]
                         #:override [override #f]
                         #:handlers [extra-handlers #f])
  (define messages (normalize-messages msg-like))
  (define req-hash (build-req-hash env messages
                                   #:stream #t
                                   #:override override))
  (define req (build-chat-request req-hash))

  (define active-stop? (if (omit? stop?) (env-stop? env) stop?))

  ;; per-call hui diao > env mo ren hui diao
  (define per-call-handlers
    (for/list ([(k v) (in-hash
                       (hasheq 'content on-content
                               'reasoning on-reasoning
                               'tool-calls on-tool-calls))]
               #:when (not (omit? v)))
      (cons k v)))

  (define default-handlers
    (for/list ([(k v) (in-hash (env-callbacks env))]
               #:unless (for/or ([h (in-list per-call-handlers)])
                          (eq? (car h) k))
               #:unless (and extra-handlers
                             (for/or ([h (in-list extra-handlers)])
                               (eq? (car h) k))))
      (cons k v)))

  (define all-handlers
    (append per-call-handlers
            (or extra-handlers '())
            default-handlers))

  (apply deepseek-chat/stream
         req
         #:stop? active-stop?
         all-handlers))


;; ============================================================
;; tool-chat-request : Zhi Xing Gong Ju, Fan Hui Xin Xiao Xi Lie Biao
;;
;; chun han shu, bu diao yong API, bu xun huan.
;;
;; can shu:
;;   tools    -- gong ju ji
;;   resp     -- env-chat fan hui de xiang ying
;;   messages -- dang qian xiao xi lie biao
;;
;; fan hui: xin de xiao xi lie biao (assistant ji lu + tool jie guo)
;;
;; yong fa:
;;   (define msgs2 (tool-chat-request default-tools resp msgs))
;;   (define resp2 (env-chat env msgs2))  ;; ji xu
;; ============================================================

(define (tool-chat-request tools resp messages)
  (define tcs (response-tool-calls resp))
  (define content (response-content resp))
  (if (not tcs)
      messages
      (for/fold ([msgs messages]) ([tc (in-list tcs)])
        (define id (tool-call-id tc))
        (define name (tool-call-func-name tc))
        (define args (tool-call-func-args tc))
        (define result (tool-dispatch tools name args))
        (append msgs
                (build-messages
                 (build-assistant-message
                  #:content content
                  #:tool_calls (list tc)))
                (build-messages
                 (build-tool-result
                  #:tool_call_id id
                  #:content result))))))

;; ============================================================
;; tool-stop-chat-request : Zhi Xing + Ting Zhi Jian Ce
;;
;; tong tool-chat-request, dan duo fan hui yi ge biao zhi biao shi
;; shi fou hai you gong ju yao diao yong.
;;
;; fan hui: (values you-gong-ju? xin-xiao-xi-lie-biao)
;;
;; yong fa:
;;   (let-values ([(more? msgs2) (tool-stop-chat-request tools resp msgs)])
;;     (if more? (loop msgs2) (display (response-content resp))))
;; ============================================================

(define (tool-stop-chat-request tools resp messages)
  (define tcs (response-tool-calls resp))
  (if (not tcs)
      (values #f messages)
      (values #t (tool-chat-request tools resp messages))))

;; ============================================================
;; tool-cancel-chat-request : Qu Xiao Gong Ju Diao Yong
;;
;; tong tool-chat-request, dan bu zhi xing gong ju, er shi ti
;; gong yi ge "yong hu zhu dong zhong zhi" xiao xi lai man zu
;; ping tai yao qiu (tool_calls bi xu you dui ying tool result).
;;
;; can shu:
;;   tools          -- gong ju ji (ke xuan, shi ji bu shi yong)
;;   resp           -- env-chat fan hui de xiang ying
;;   messages       -- dang qian xiao xi lie biao
;;   #:cancel-message -- zi ding yi qu xiao xiao xi (mo ren "用户主动终止工具调用")
;;
;; fan hui: xin de xiao xi lie biao (assistant ji lu + tool jie guo = qu xiao xiao xi)
;;
;; yong fa:
;;   (define resp (env-chat env "yun xing whoami"))
;;   ;; yong hu jue ding qu xiao
;;   (define msgs2 (tool-cancel-chat-request default-tools resp msgs))
;;   (define resp2 (env-chat env msgs2))
;; ============================================================

(define (tool-cancel-chat-request tools resp messages
                                  #:cancel-message [cancel-message "用户主动终止工具调用"])
  (define tcs (response-tool-calls resp))
  (define content (response-content resp))
  ;; DeepSeek 要求: 如果响应中带 reasoning_content，回传时必须带上
  (define reasoning (response-reasoning resp))
  (if (not tcs)
      messages
      (for/fold ([msgs messages]) ([tc (in-list tcs)])
        (define id (tool-call-id tc))
        (append msgs
                (build-messages
                 (build-assistant-message
                  #:content content
                  #:reasoning_content reasoning
                  #:tool_calls (list tc)))
                (build-messages
                 (build-tool-result
                  #:tool_call_id id
                  #:content cancel-message))))))

;; ============================================================
;; env-print : Da Yin Huan Jing Xin Xi
;; ============================================================

(define (env-print e)
  (display clr-cyan)
  (display "=== DeepSeek Huan Jing ===")
  (display format-reset)
  (newline)
  (display "  Mo Xing: ")
  (displayln (or (env-model e) "(not set)"))
  (display "  Can Shu: ")
  (displayln (if (zero? (hash-count (env-params e)))
                 "(none)"
                 (hash->list (env-params e))))
  (display "  Gong Ju: ")
  (display clr-green)
  (display (if (env-tools e)
               (string-join (tools-names (env-tools e)) ", ")
               "(none)"))
  (display format-reset)
  (newline)
  (display "  Hui Diao: ")
  (displayln (if (zero? (hash-count (env-callbacks e)))
                 "(none)"
                 (hash-keys (env-callbacks e))))
  (display clr-cyan)
  (display "================")
  (display format-reset)
  (newline))
