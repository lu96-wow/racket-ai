#lang racket
;; ============================================================
;; test-tool-less.rkt
;;
;; 测试：触发工具调用后，返回 tool result 内容为
;;       "用户主动终止工具调用"，验证 API 是否接受
;;
;; 预期：API 正常响应（无报错），因为平台要求 tool_calls
;;       后必须有 tool 结果，内容可以是任意字符串。
;; ============================================================

(require "env/deepseek-env.rkt"
         "api-config/deepseek.rkt"
         "tools/deepseek-base-tool.rkt")

;; 环境：带工具的 DeepSeek
(define my-env
  (make-env #:model deepseek-v4-flash #:max_tokens 4096
            #:tools default-tools))

(define (main)
  (printf "\n===== test-tool-less: 触发工具后返回用户主动终止工具调用 =====\n\n")

  ;; 第 1 步：发送会触发工具调用的消息，拿到含 tool_calls 的响应
  (printf "--- 第 1 步：发送 '运行 whoami 命令' ---\n")
  (define resp1 (env-chat my-env "运行 whoami 命令"))
  (define tcs (response-tool-calls resp1))

  (unless tcs
    (error "✗ 工具调用未被触发，无法继续测试"))

  (printf "✓ 工具调用被触发:\n")
  (for ([tc (in-list tcs)])
    (printf "  ~a(id=~a, args=~a)\n"
            (tool-call-func-name tc)
            (tool-call-id tc)
            (tool-call-func-args tc)))

  ;; 第 2 步：构造消息，包含 tool result（内容为"用户主动终止工具调用"）
  ;; 消息结构: [user-msg, assistant-msg(含 tool_calls), tool-result-msg, new-user-msg]
  (printf "\n--- 第 2 步：构造带 tool result(用户主动终止工具调用) 的消息列表 ---\n")
  (define user-msg1   (build-user-message #:content "运行 whoami 命令"))
  (define assistant-msg
    (build-assistant-message #:content (or (response-content resp1) "")
                             #:tool_calls tcs))

  ;; 为每个 tool call 生成一个 tool result，内容为"用户主动终止工具调用"
  (define tool-result-msgs
    (for/list ([tc (in-list tcs)])
      (build-tool-result #:tool_call_id (tool-call-id tc)
                         #:content "用户主动终止工具调用")))

  (define user-msg2   (build-user-message #:content "继续，帮我做另一件事"))

  (define msgs-with-tool-result
    (append (build-messages user-msg1 assistant-msg)
            tool-result-msgs
            (build-messages user-msg2)))

  (printf "消息列表（共 ~a 条，包含 tool result）：\n"
          (length msgs-with-tool-result))
  (for ([m msgs-with-tool-result] [i (in-naturals 1)])
    (printf "  ~a. role=~a, tool_call_id=~a, content=~a\n"
            i
            (hash-ref m 'role)
            (hash-ref m 'tool_call_id "✗")
            (let ([c (hash-ref m 'content "")])
              (if (> (string-length c) 30)
                  (string-append (substring c 0 30) "...")
                  c))))

  ;; 第 3 步：发送请求 —— 预期正常响应
  (printf "\n--- 第 3 步：发送请求（预期正常响应）---\n")
  (with-handlers ([exn:fail? (lambda (e)
                               (printf "\n✗ 捕获到错误（不符合预期）:\n  ~a\n" (exn-message e))
                               (printf "\n结论：即使提供了 tool result，API 仍然报错。\n"))])
    (define resp2 (env-chat my-env msgs-with-tool-result))
    (printf "✓ API 正常响应！\n")
    (printf "  模型: ~a\n" (response-model resp2))
    (printf "  内容: ~a\n" (response-content resp2))
    (printf "\n结论：提供 tool result（内容为'用户主动终止工具调用'）可满足平台要求。\n")
    (printf "提示：可以基于此实现一个函数，在用户主动终止工具调用时自动插入此消息。\n")))

(main)
