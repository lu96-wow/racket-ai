#lang racket
;; ============================================================
;; test-tool-cancel.rkt
;;
;; 测试：使用 tool-cancel-chat-request 主动终止工具调用
;;
;; 对比 test-tool-less.rkt（手动构造终止消息）：
;;   这里用 tool-cancel-chat-request 一行搞定
;; ============================================================

(require "env/deepseek-env.rkt"
         "api-config/deepseek.rkt"
         "tools/deepseek-base-tool.rkt")

(define my-env
  (make-env #:model deepseek-v4-flash #:max_tokens 4096
            #:tools default-tools))

(define (demo1)
  "基本用法：取消工具后继续对话"
  (printf "\n===== demo1: tool-cancel-chat-request 基本用法 =====\n\n")

  ;; 触发工具调用
  (define resp (env-chat my-env "运行 whoami 命令"))
  (define tcs (response-tool-calls resp))
  (unless tcs (error "✗ 未触发工具调用"))

  (printf "✓ 工具调用被触发: ~a\n"
          (string-join (for/list ([tc tcs]) (tool-call-func-name tc)) ", "))

  ;; 用户决定取消 — 一行搞定！
  (define msgs (build-messages (build-user-message #:content "运行 whoami 命令")))
  (define msgs2 (tool-cancel-chat-request default-tools resp msgs))

  (printf "✓ 取消后消息列表（共 ~a 条）：\n" (length msgs2))
  (for ([m msgs2] [i (in-naturals 1)])
    (printf "  ~a. role=~a~a\n" i (hash-ref m 'role)
            (let ([tc (hash-ref m 'tool_call_id #f)])
              (if tc (format ", tool_call_id=~a" (substring tc 0 30)) ""))))

  ;; 继续对话
  (define msgs3 (append msgs2 (build-messages (build-user-message #:content "继续，帮我做另一件事"))))
  (define resp2 (env-chat my-env msgs3))
  (printf "\n✓ 继续对话成功: ~a\n" (response-content resp2)))

(define (demo2)
  "循环中的取消模式"
  (printf "\n===== demo2: 循环中使用 tool-cancel-chat-request =====\n\n")

  (let loop ([msgs (build-messages (build-user-message #:content "运行 whoami 命令"))]
             [n 0])
    (when (>= n 3)
      (printf "达到最大次数，停止\n")
      (void))
    (let ()
      (printf "--- 轮次 ~a ---\n" (add1 n))
      (define resp (env-chat my-env msgs))
      (define tcs (response-tool-calls resp))

      (cond
        [(not tcs)
         (printf "✓ 无工具调用，结束循环\n")
         (printf "  内容: ~a\n" (response-content resp))]
        [else
         (printf "工具调用: ~a\n"
                 (string-join (for/list ([tc tcs]) (tool-call-func-name tc)) ", "))
         ;; 询问用户是否执行（模拟）
         (printf "是否执行工具？[y/n]: n (模拟用户取消)\n")
         (define msgs2 (tool-cancel-chat-request default-tools resp msgs
                                                 #:cancel-message "用户选择取消"))
         (printf "取消完成，继续下一轮...\n")
         (loop msgs2 (add1 n))]))))

(define (demo3)
  "自定义取消消息"
  (printf "\n===== demo3: 自定义取消消息 =====\n\n")

  (define resp (env-chat my-env "运行 whoami 命令"))
  (define tcs (response-tool-calls resp))
  (unless tcs (error "✗ 未触发工具调用"))

  (define msgs (build-messages (build-user-message #:content "运行 whoami 命令")))
  (define msgs2 (tool-cancel-chat-request default-tools resp msgs
                                          #:cancel-message "用户取消了操作"))

  (printf "自定义取消消息后，tool result content:\n  ~a\n"
          (hash-ref (list-ref msgs2 1) 'content))
  (printf "\n✓ 自定义消息生效\n"))

(demo1)
(demo2)
(demo3)

(printf "\n所有测试完成。\n")
