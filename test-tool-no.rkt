#lang racket
;; ============================================================
;; test-tool-no.rkt
;;
;; 测试：触发工具调用后，不返回 tool result，直接发新用户消息
;;
;; 预期：API 报错（400），因为 OpenAI 兼容平台要求
;;       tool_calls 之后必须有对应的 tool 角色结果消息。
;;       如果缺少 tool result，API 会拒绝请求。
;; ============================================================

(require "env/deepseek-env.rkt"
         "api-config/deepseek.rkt"
         "tools/deepseek-base-tool.rkt")

;; 环境：带工具的 DeepSeek
(define my-env
  (make-env #:model deepseek-v4-flash #:max_tokens 4096
            #:tools default-tools))

(define (main)
  (printf "\n===== test-tool-no: 触发工具后不返回 tool result =====\n\n")

  ;; 第 1 步：发送会触发工具调用的消息，拿到含 tool_calls 的响应
  (printf "--- 第 1 步：发送 '运行 whoami 命令' ---\n")
  (define resp1 (env-chat my-env "运行 whoami 命令"))
  (define tcs (response-tool-calls resp1))

  (unless tcs
    (error "✗ 工具调用未被触发，无法继续测试"))

  (printf "✓ 工具调用被触发:\n")
  (for ([tc (in-list tcs)])
    (printf "  ~a(~a)\n" (tool-call-func-name tc) (tool-call-func-args tc)))

  ;; 第 2 步：构造消息时故意不包含 tool result
  ;; 消息结构: [user-msg, assistant-msg(含 tool_calls), new-user-msg]
  (printf "\n--- 第 2 步：构造缺少 tool result 的消息列表 ---\n")
  (define user-msg1   (build-user-message #:content "运行 whoami 命令"))
  (define assistant-msg
    (build-assistant-message #:content (or (response-content resp1) "")
                             #:tool_calls tcs))
  (define user-msg2   (build-user-message #:content "继续，帮我做另一件事"))

  (define msgs-without-tool-result
    (build-messages user-msg1 assistant-msg user-msg2))

  (printf "消息列表（共 ~a 条，缺少 tool result）：\n"
          (length msgs-without-tool-result))
  (for ([m msgs-without-tool-result] [i (in-naturals 1)])
    (printf "  ~a. role=~a, has-tool_calls=~a\n"
            i
            (hash-ref m 'role)
            (if (hash-has-key? m 'tool_calls) "✓" "✗")))

  ;; 第 3 步：发送请求 —— 预期 API 会报 400 错误
  (printf "\n--- 第 3 步：发送请求（预期 API 报错）---\n")
  (with-handlers ([exn:fail? (lambda (e)
                               (printf "\n✓ 捕获到预期错误:\n  ~a\n" (exn-message e))
                               (printf "\n结论：缺少 tool result 时 API 报错，符合预期。\n")
                               (printf "提示：需要实现一个函数，在用户主动终止工具调用时，\n")
                               (printf "自动生成 tool result（内容如'用户主动终止工具调用'）来满足平台要求。\n"))])
    (define resp2 (env-chat my-env msgs-without-tool-result))
    (printf "!!! 未报错！响应内容: ~a\n" (response-content resp2))
    (printf "\n结论：未报错，不符合预期（可能当前 API 不强制要求 tool result）。\n")))

(main)
