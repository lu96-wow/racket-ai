#lang racket

(require "env/deepseek-env.rkt"
         "api-config/deepseek.rkt"
         "tools/deepseek-base-tool.rkt")

(define my-env
  (make-env #:model deepseek-v4-flash #:max_tokens 4096
            #:on-content (lambda (c) (display c) (flush-output))))

(define my-env-t
  (make-env #:model deepseek-v4-flash #:max_tokens 4096
            #:thinking #t #:reasoning-effort "high"
            #:on-content (lambda (c) (display c) (flush-output))
            #:on-reasoning (lambda (r) (display r))))

(define my-env-tools
  (make-env #:model deepseek-v4-flash #:max_tokens 8192
            #:tools default-tools
            #:on-content (lambda (c) (display c) (flush-output))))

(define (demo1)
  (printf "\n===== 1 =====\n\n")
  (define resp (env-chat my-env "yong yi ju hua jie shao ni zi ji."))
  (printf "~a\n" (response-content resp))
  (define u (response-usage resp))
  (when u (printf "in=~a out=~a\n"
                  (hash-ref u (quote prompt_tokens))
                  (hash-ref u (quote completion_tokens)))))

(define (demo2)
  (printf "\n===== 2 =====\n\n")
  (env-chat/stream my-env "cong 1 shu dao 5")
  (printf "\n"))

(define (demo3)
  (printf "\n===== 3 =====\n\n")
  (define buf "")
  (env-chat/stream my-env-t "9.9 he 9.11?"
                   #:on-content (lambda (c) (set! buf (string-append buf c))))
  (printf "\nzong: ~a\n" buf))

(define (demo4)
  (printf "\n===== 4 =====\n\n")
  (define resp (env-chat my-env-tools "yun xing whoami ming ling."))
  (define tcs (response-tool-calls resp))
  (when (response-content resp) (printf "~a\n" (response-content resp)))
  (when tcs
    (for ([tc (in-list tcs)])
      (printf "  ~a => ~a\n" (tool-call-func-name tc)
              (string-trim (tool-dispatch default-tools
                                          (tool-call-func-name tc) (tool-call-func-args tc)))))))

(define (demo5)
  (printf "\n===== 5 =====\n\n")
  (let loop ([msgs (build-messages
                    (build-user-message #:content "yun xing whoami"))]
             [n 0])
    (if (>= n 5)
        (printf "stop\n")
        (let ()
          (printf "--- ~a ---\n" (add1 n))
          (define resp (env-chat my-env-tools msgs))
          (let-values ([(m ms) (tool-stop-chat-request default-tools resp msgs)])
            (if m (loop ms (add1 n)) (printf "done\n")))))))

;;(demo1)
;;(demo2)
;; (demo3)
(demo4)
;; (demo5)
