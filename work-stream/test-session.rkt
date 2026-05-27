#lang racket
;; ============================================================
;; work-stream/test-session.rkt — 测试分支对话流
;;
;; 测试场景：
;;   1. 触发工具调用（shell），验证工具循环 + 流式输出
;;   2. 打整理点 + 执行整理
;;   3. 分支 + 整理点自动清空
;; ============================================================

(require "session.rkt"
         "../env/deepseek-env.rkt"
         "../api-config/deepseek.rkt"
         "../tools/deepseek-base-tool.rkt")

(define env
  (make-env #:model deepseek-v4-flash #:max_tokens 8192
            #:tools default-tools
            #:on-content (lambda (c) (display c) (flush-output))))

(define (demo-tool)
  "触发工具调用，验证工具自动执行 + 流式输出"
  (printf "\n===== demo-tool: 工具调用 + 流式 =====\n\n")
  (define sess (make-session env))

  ;; 必定触发 shell 工具
  (printf "--- 第 1 轮: 运行 whoami ---\n")
  (let*-values ([(s1 r1)
                 (session-chat sess "运行 whoami 命令")])
    (printf "\n\n响应中工具调用: ~a\n"
            (if (response-tool-calls r1) "有 ✓" "无"))
    (session-print-path s1)

    ;; 第二问，验证上下文
    (printf "\n--- 第 2 轮: 基于上下文的提问 ---\n")
    (let*-values ([(s2 r2)
                   (session-chat s1 "我当前的工作目录是什么？")])
      (printf "\nAI: ~a\n" (response-content r2))
      (session-print-tree s2))
    s1))

(define (demo-organize)
  "整理点工作流"
  (printf "\n===== demo-organize: 整理点 =====\n\n")
  (define sess (make-session env))

  (printf "--- 第 1 轮: 触发工具 ---\n")
  (let*-values ([(s1 r1) (session-chat sess "运行 whoami 命令")])
    (printf "\n")

    (printf "--- 第 2 轮: 触发工具 ---\n")
    (let*-values ([(s2 r2) (session-chat s1 "运行 pwd 命令")])
      (printf "\n")

      (printf "--- 第 3 轮: 打整理点 📌 ---\n")
      (let*-values ([(s3 _) (session-organize s2)])
        (printf "整理点: ~a\n" (if (session-organize? s3) "活跃" "无"))

        (printf "\n--- 第 4 轮: 继续再问一个工具问题 ---\n")
        (let*-values ([(s4 r4) (session-chat s3 "运行 ls -la 命令")])
          (printf "\n")

          (printf "--- 第 5 轮: 执行整理 🔄 ---\n")
          (let*-values ([(s5 r5) (session-organize s4)])
            (when r5
              (printf "AI 整理结果: ~a\n" (response-content r5)))
            (printf "整理点: ~a\n" (if (session-organize? s5) "活跃" "无"))
            (session-print-tree s5)
            s5))))))

(define (demo-branch)
  "分支 + 整理点自动清空"
  (printf "\n===== demo-branch: 分支 + 整理点清空 =====\n\n")
  (define sess (make-session env))

  ;; 路径 A
  (printf "--- 路径 A ---\n")
  (let*-values ([(s1 r1) (session-chat sess "运行 whoami 命令")])
    (printf "\n")

    ;; 打整理点
    (printf "--- 打整理点 ---\n")
    (let*-values ([(s2 _) (session-organize s1)])
      (printf "整理点: ~a\n" (if (session-organize? s2) "活跃" "无"))

      ;; 分叉到 B
      (printf "\n--- 从 root 分支到路径 B ---\n")
      (let*-values ([(s3 r3) (session-branch s2 "运行 pwd 命令")])
        (printf "\n")
        (printf "整理点: ~a (应被清空)\n"
                (if (session-organize? s3) "活跃 ⚠️" "已清空 ✓"))
        (session-print-tree s3)))))

;;
;; 运行
;;
(define result1 (demo-tool))
(printf "\n========================================\n")
(define result2 (demo-organize))
(printf "\n========================================\n")
(define result3 (demo-branch))
(printf "\n========================================\n")
(printf "\n所有测试完成。\n")
