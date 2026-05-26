#lang racket
;; ============================================================
;; test-chinese-tool-deepseek.rkt
;;
;; 测试：全中文输入 + 中文工具参数名
;; 目的：观察 DeepSeek 是否用中文思考，并实际调用一次工具
;; 使用 env API 流式回显思考过程
;; ============================================================

(require "env/deepseek-env.rkt"
         "api-config/deepseek.rkt"
         "tools/deepseek-base-tool-chinese.rkt"
         "format-color/core.rkt")

;; 环境：思考 + 中文工具（默认回调不设，由 env-chat/stream 传参覆盖）
(define my-env
  (make-env #:model deepseek-v4-flash #:max_tokens 8192
            #:thinking #t #:reasoning-effort "high"
            #:tools default-tools))

(define (main)
  (printf "\n===== 测试：中文工具调用（流式回显思考过程）=====\n\n")

  ;; 流式调用：实时显示思考过程 + 回复内容
  ;; env-chat/stream 现在同时走回调并累积返回完整 response
  (define resp (env-chat/stream my-env
                                "写一首英文诗，保存到 spring-poem.txt 文件中。"
                                #:on-reasoning (lambda (r)
                                                 ;; 思考内容用深色/斜体显示，与回复区分
                                                 (display format-dim)
                                                 (display r)
                                                 (display format-reset)
                                                 (flush-output))
                                #:on-content (lambda (c)
                                               (display c)
                                               (flush-output))))

  (newline)

  ;; 用返回的完整 response + env 封装的 tool-chat-request
  (define tcs (response-tool-calls resp))
  (when tcs
    (printf "\n工具调用:\n")
    (for ([tc (in-list tcs)])
      (printf "  ~a ~a => "
              (tool-call-func-name tc) (tool-call-func-args tc))
      (define result (tool-dispatch default-tools
                                    (tool-call-func-name tc)
                                    (tool-call-func-args tc)))
      (printf "~a\n" (string-trim result))))

  (printf "\n测试完成。\n"))

(main)
