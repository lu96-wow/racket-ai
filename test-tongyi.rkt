#lang racket

(require "env/tongyi-env.rkt"
         "api-config/tongyi.rkt"
         "tools/deepseek-base-tool.rkt")

(define key (getenv "DASHSCOPE_API_KEY"))
(unless key
  (printf "export DASHSCOPE_API_KEY=...\n")
  (exit 1))

(define my-env
  (make-env #:model qwen-flash
            #:max_tokens 4096
            #:on-content (lambda (c) (display c) (flush-output))))

;; 1. fei liu shi
(printf "\n===== 1 =====\n")
(define resp (env-chat my-env "yong yi ju hua jie shao ni zi ji."))
(printf "~a\n" (response-content resp))

;; 2. liu shi
(printf "\n===== 2 =====\n")
(env-chat/stream my-env "cong 1 shu dao 5")
(printf "\n")

;; 3. gong ju
(printf "\n===== 3 =====\n")
(define e (env-set my-env #:tools default-tools))
(define resp2 (env-chat e "yun xing whoami ming ling."))
(printf "~a\n" (response-content resp2))
(define tcs (response-tool-calls resp2))
(when tcs
  (for ([tc (in-list tcs)])
    (printf "  -> ~a\n" (tool-call-func-name tc))))
