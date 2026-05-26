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

  ;; 累积合并流式分段送达的工具调用（按 index 合并）
  (define tool-call-acc (hash))

  ;; 流式调用：实时显示思考过程 + 回复内容
  (env-chat/stream my-env
    "写一首关于春天的英文短诗（4行），保存到 spring-poem.txt 文件中。"
    #:on-reasoning (lambda (r)
                     ;; 思考内容用深色/斜体显示，与回复区分
                     (display format-dim)
                     (display r)
                     (display format-reset)
                     (flush-output))
    #:on-content (lambda (c)
                   (display c)
                   (flush-output))
    #:on-tool-calls (lambda (tcs)
                      ;; merge-tool-calls 来自 tools/tool.rkt，
                      ;; 合并按 index 分片的 tool call 增量
                      (set! tool-call-acc
                            (merge-tool-calls tool-call-acc tcs))))

  (newline)

  ;; 从累积的 hash 中提取完成（有 function.name 的）tool call
  (define tcs
    (for/list ([(k v) (in-hash tool-call-acc)]
               #:when (and (hash-has-key? v 'function)
                           (hash-has-key? (hash-ref v 'function) 'name)))
      v))

  (when (pair? tcs)
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
