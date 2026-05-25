#lang racket

;; ============================================================
;; test.rkt — DeepSeek API 四种调用模式示例
;;
;; 运行方式:
;;   export DEEPSEEK_API_KEY="sk-..."
;;   racket test.rkt
;;
;; 默认只运行示例 1、2（无需工具），取消注释 main 底部的
;; 行可启用示例 3、4、5。
;; ============================================================

(require "api-platform/deepseek/chat.rkt"
         "api-platform/deepseek/json-build-parse.rkt"
         "api-config/deepseek.rkt"
         "tools/deepseek-base-tool.rkt")

;; ============================================================
;; 辅助: 按行打印
;; ============================================================

(define (show-line prefix str)
  (for ([line (in-list (string-split str "\n"))])
    (printf "  ~a ~a\n" prefix line)))

;; ============================================================
;; 示例 1: 非流式 — 最简单的对话
;;
;;   build-user-message
;;   → build-chat-request (不传 #:stream)
;;   → deepseek-chat
;;   → response-content / response-usage
;; ============================================================

(define (demo-non-stream)
  (printf "\n══════ 示例 1: 非流式普通对话 ══════\n\n")

  ;; 1. 构建消息列表
  (define messages
    (build-messages (build-user-message #:content "用一句话介绍你自己。")))

  ;; 2. 构建请求 (非流式: 不传 #:stream)
  (define req (build-chat-request (hasheq 'model deepseek-v4-flash
                                          'messages messages
                                          'max_tokens 256)))

  ;; 3. 发送，等待完整响应
  (define resp (deepseek-chat req))

  ;; 4. 解析
  (printf "模型: ~a\n" (response-model resp))
  (printf "ID:   ~a\n" (response-id resp))
  (show-line "→" (response-content resp))

  ;; 5. 用量
  (define usage (response-usage resp))
  (when usage
    (printf "用量: input=~a  output=~a\n"
            (hash-ref usage 'prompt_tokens)
            (hash-ref usage 'completion_tokens))))

;; ============================================================
;; 示例 2: 流式 — 逐块显示内容
;;
;;   build-user-message
;;   → build-chat-request #:stream #t
;;   → deepseek-chat/stream (回调分发 content / done)
;; ============================================================

(define (demo-stream)
  (printf "\n══════ 示例 2: 流式普通对话 ══════\n\n")

  (define messages
    (build-messages (build-user-message #:content "从1数到5，用逗号分隔。")))

  (define req (build-chat-request (hasheq 'model deepseek-v4-flash
                                          'messages messages
                                          'stream #t
                                          'max_tokens 256)))

  (define buf "")
  (deepseek-chat/stream req
                        (cons 'content (λ (c) (set! buf (string-append buf c))
                                         (display c)
                                         (flush-output)))))

;; ============================================================
;; 示例 3: 流式 + 思考模式 (reasoning)
;;
;;   build-thinking #t
;;   → delta-reasoning (灰色) + delta-content 分别显示
;; ============================================================

(define (demo-stream-thinking)
  (printf "\n══════ 示例 3: 流式 + 思考模式 ══════\n\n")

  (define messages
    (build-messages (build-user-message #:content "9.9和9.11哪个大？一步步思考。")))

  (define req (build-chat-request (hasheq 'model deepseek-v4-flash
                                          'messages messages
                                          'stream #t
                                          'max_tokens 4096
                                          'thinking #t
                                          'reasoning_effort "high")))

  (define acc-rc "")
  (define acc-cc "")
  (deepseek-chat/stream req
                        (cons 'reasoning (λ (r) (set! acc-rc (string-append acc-rc r))
                                           (display "\x1b[90m")
                                           (display r)
                                           (display "\x1b[0m")
                                           (flush-output)))
                        (cons 'content   (λ (c) (set! acc-cc (string-append acc-cc c))
                                           (display c)
                                           (flush-output))))
  (printf "\n\n=== 汇总 ===\n")
  (show-line "思考:" acc-rc)
  (show-line "回答:" acc-cc))

;; ============================================================
;; 示例 4: 非流式 + 工具调用
;;
;;   build-chat-request #:tools (tools-schemas default-tools)
;;   → deepseek-chat
;;   → response-content + response-tool-calls
;;   → tool-dispatch 执行每个工具
;; ============================================================

(define (demo-tools-nonstream)
  (printf "\n══════ 示例 4: 非流式 + 工具调用 ══════\n\n")

  (define messages
    (build-messages (build-user-message #:content "运行 whoami 命令。")))

  (define req (build-chat-request (hasheq 'model deepseek-v4-flash
                                          'messages messages
                                          'max_tokens 4096
                                          'tools (tools-schemas default-tools))))

  (define resp (deepseek-chat req))
  (printf "模型: ~a\n" (response-model resp))

  ;; 非流式响应: content + tool_calls 都在 message 里
  (define content (response-content resp))
  (define tcs (response-tool-calls resp))

  (when content
    (show-line "→" content))

  (when tcs
    (for ([tc (in-list tcs)])
      (define name  (tool-call-func-name tc))
      (define args  (tool-call-func-args tc))
      (define id    (tool-call-id tc))
      (printf "  工具调用: ~a (~a)\n" name args)
      (define result (tool-dispatch default-tools name args))
      (printf "  结果: ~a\n" (string-trim result)))))

;; ============================================================
;; 示例 5: 流式 + 工具调用 (自动循环)
;;
;; 关键流程:
;;   1. 发送消息 + all-tool-schemas
;;   2. 流式读取，同时累积 delta-content 和 delta-tool-calls
;;   3. merge-tool-calls 合并增量 tool_calls
;;   4. 流结束 → 排序合并后的 tool_calls
;;   5. 构建 assistant 消息 (含 content + tool_calls)
;;   6. 依次执行 tool-dispatch → build-tool-result
;;   7. 追加到消息列表 → 递归请求下一轮
;;   8. 重复直到助手返回纯文本 (无工具调用)
;; ============================================================

(define (demo-tools-stream)
  (printf "\n══════ 示例 5: 流式 + 工具调用 (自动循环) ══════\n\n")

  (let loop ([messages
              (build-messages
               (build-user-message #:content
                (string-append "帮我做两件事：\n"
                               "1) 运行 uname -a\n"
                               "2) 读取当前目录下的 env.sh")))]
             [turn 0])

    (if (>= turn 5)
        (printf "\n[达到最大轮数 5，终止]\n")
        (let ()
          (printf "--- 第 ~a 轮 ---\n" (add1 turn))
          (define req (build-chat-request (hasheq 'model deepseek-v4-flash
                                                  'messages messages
                                                  'stream #t
                                                  'max_tokens 8192
                                                  'tools (tools-schemas default-tools))))
          (define acc-cc "")
          (define acc-rc "")
          (define tc-hash (hasheq))

          (deepseek-chat/stream req
                                (cons 'content (λ (c) (set! acc-cc (string-append acc-cc c))
                                                 (display c)
                                                 (flush-output)))
                                (cons 'reasoning (λ (r) (set! acc-rc (string-append acc-rc r))))
                                (cons 'tool-calls (λ (tcs) (set! tc-hash (merge-tool-calls tc-hash tcs)))))
          (printf "\n")
          (define sorted-keys (sort (hash-keys tc-hash) <))
          (define tool-calls
            (for/list ([k (in-list sorted-keys)])
              (hash-ref tc-hash k)))
          (define asst-msg
            (if (null? tool-calls)
                (build-assistant-message #:content acc-cc #:reasoning_content acc-rc)
                (build-assistant-message #:content acc-cc #:reasoning_content acc-rc #:tool_calls tool-calls)))
          (define new-msgs (append messages (build-messages asst-msg)))
          (cond
            [(null? tool-calls)
             (printf "\n=== 最终回答 ===\n")
             (show-line "→" acc-cc)]
            [else
             (printf "\n[检测到 ~a 个工具调用，开始执行...]\n"
                     (length tool-calls))
             (for ([tc (in-list tool-calls)])
               (define id    (tool-call-id tc))
               (define name  (tool-call-func-name tc))
               (define args  (tool-call-func-args tc))
               (printf "  工具: ~a (~a)\n" name args)
               (define result (tool-dispatch default-tools name args))
               (printf "  结果: ~a\n" (string-trim result))
               (set! new-msgs (append new-msgs
                                      (build-messages (build-tool-result #:tool_call_id id #:content result)))))
             (printf "\n[继续下一轮...]\n\n")
             (loop new-msgs (add1 turn))])))))

;; ============================================================
;; 主入口
;; ============================================================

(printf "╔══════════════════════════════════════════╗\n")
(printf "║      DeepSeek API 调用模式示例          ║\n")
(printf "╚══════════════════════════════════════════╝\n\n")

(define key (getenv "DEEPSEEK_API_KEY"))
(printf "DEEPSEEK_API_KEY=~a\n\n"
        (if key
            (string-append (substring key 0 12) "...")
            "(未设置)"))
(unless key
  (printf "⚠ 请先设置环境变量:\n")
  (printf "   export DEEPSEEK_API_KEY=\"sk-...\"\n")
  (exit 1))

;; 默认运行: 示例 1 + 2
;;(demo-non-stream)
;;(demo-stream)

;; 取消注释即可运行:
(demo-stream-thinking)
;;(demo-tools-nonstream)   ;; 示例 4 — 非流式 + 工具
(demo-tools-stream)      ;; 示例 5 — 流式 + 工具自动循环

(printf "\n完成。取消注释 main 底部的行运行更多示例。\n")
