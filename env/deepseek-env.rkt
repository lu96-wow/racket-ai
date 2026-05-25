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

(require "../api-platform/deepseek/chat.rkt"
         "../api-platform/deepseek/json-build-parse.rkt"
         "../tools/tool.rkt")

(provide
 ;; huan jing API
 make-env env-set env?
 env-model env-params env-tools env-callbacks env-stop?
 env-chat env-chat/stream
 env-print

 ;; xiang ying jie xi (re-export)
 response-id response-model response-usage
 response-content response-tool-calls
 response-first-choice
 tool-call-id tool-call-func-name tool-call-func-args)

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
                         #:override [override #f])
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
                          (eq? (car h) k)))
      (cons k v)))

  (define all-handlers (append per-call-handlers default-handlers))

  (apply deepseek-chat/stream
         req
         #:stop? active-stop?
         all-handlers))

;; ============================================================
;; env-print : Da Yin Huan Jing Xin Xi
;; ============================================================

(define (env-print e)
  (printf "=== DeepSeek Huan Jing ===\n")
  (printf "  Mo Xing:     ~a\n" (or (env-model e) "(not set)"))
  (printf "  Can Shu:     ~a\n"
          (if (zero? (hash-count (env-params e)))
              "(none)"
              (hash->list (env-params e))))
  (printf "  Gong Ju:     ~a\n"
          (if (env-tools e)
              (string-join (tools-names (env-tools e)) ", ")
              "(none)"))
  (printf "  Hui Diao:    ~a\n"
          (if (zero? (hash-count (env-callbacks e)))
              "(none)"
              (hash-keys (env-callbacks e))))
  (printf "================\n"))
