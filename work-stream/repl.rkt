#lang racket
;; ============================================================
;; work-stream/repl.rkt — 命令行流式对话应用
;;
;; 命令:
;;   普通输入      → 对话（流式+工具循环）
;;   /branch <t>  → 从当前节点分叉新分支
;;   /mark        → 标记整理点
;;   /organize    → 执行整理
;;   /move <n>    → 移动到第 n 个叶子节点
;;   /tree        → 打印整棵树
;;   /path        → 打印当前路径
;;   /leaves      → 列出所有叶子节点
;;   /help        → 帮助
;;   /quit        → 退出
;; ============================================================

(require "session.rkt"
         "../env/deepseek-env.rkt"
         "../api-config/deepseek.rkt"
         "../tools/deepseek-base-tool-chinese.rkt"
         "../history/history.rkt"
         "../format-color/core.rkt"
         "../format-color/styles.rkt")

(define env
  (make-env #:model deepseek-v4-flash #:max_tokens 8192
            #:tools default-tools
            #:thinking #t
            #:reasoning-effort "high"))

;; ============================================================
;; 流式回调
;; ============================================================

(define (on-reasoning r)
  (display format-dim)
  (display r)
  (display format-reset)
  (flush-output))

(define (on-content c)
  (display c)
  (flush-output))

;; on-tool-calls 回调从流式片段接收数据（碎片化的）
;; 完整的工具调用信息从 session-chat/session-branch 返回的
;; resp 中通过 response-tool-calls 获取。
;; REPL 层直接用 tool-confirm 来显示和确认工具调用。
(define (on-tool-calls tcs)
  (void))  ;; 不处理流式片段，等完整响应再处理

;; ============================================================
;; tool-confirm : 用户确认工具调用
;; 返回 #t 执行，#f 取消
;; ============================================================

(define (tool-confirm name args)
  (display clr-yellow)
  (printf "🛠 调用工具: ~a\n" name)
  (printf "   参数: ~a\n" args)
  (display "执行？[Y/n] ")
  (display format-reset)
  (flush-output)
  (define line (read-line))
  (not (string-prefix? (string-trim line) "n")))




;; ============================================================
;; 显示帮助
;; ============================================================

(define (show-help)
  (display clr-cyan)
  (display "命令:\n")
  (display "  <文本>        对话（流式 + 工具自动循环）\n")
  (display "  /branch <t>   以新输入分叉\n")
  (display "  /branch-same  以当前输入分叉（复制节点）\n")
  (display "  /mark         标记整理点\n")
  (display "  /organize     执行整理\n")
  (display "  /node <n>     查看路径上第 n 个节点详情\n")
  (display "  /move <n>     移动到第 n 个叶子\n")
  (display "  /tree         打印树\n")
  (display "  /path         打印当前路径\n")
  (display "  /leaves       列出叶子\n")
  (display "  /help         显示帮助\n")
  (display "  /quit         退出\n")
  (display format-reset))

;; ============================================================
;; 显示节点详情
;; ============================================================

(define (show-node sess n)
  (define node (session-find-node sess n))
  (if node
      (let ()
        (define inp (history-node-input node))
        (define out-msgs (history-node-output node))
        (define depth
          (let loop ([n node] [d 0])
            (if (history-node-parent n)
                (loop (history-node-parent n) (add1 d))
                d)))
        (printf "节点 ~a (深度 ~a):\n" n depth)
        (printf "  子节点数: ~a\n" (length (history-node-children node)))
        (printf "  是叶子? ~a\n" (null? (history-node-children node)))
        (when inp
          (printf "  输入: ~a\n" (hash-ref inp 'content "")))
        (when out-msgs
          (printf "  输出 (~a 条消息):\n" (length out-msgs))
          (for ([m out-msgs] [i (in-naturals 1)])
            (define role (hash-ref m 'role))
            (define content (hash-ref m 'content ""))
            (define tc (hash-ref m 'tool_calls #f))
            (printf "    ~a. [~a] " i role)
            (if tc
                (printf "tool_calls: ~a"
                        (string-join
                         (for/list ([t tc])
                           (hash-ref (hash-ref t 'function) 'name "?"))
                         ", "))
                (printf "~a" (if (> (string-length content) 80)
                                (string-append (substring content 0 80) "...")
                                content)))
            (newline)))
        (printf "  整理点: ~a\n" (if (eq? node (session-organize-node sess)) "✓" "✗"))
        (printf "  当前位置: ~a\n" (if (eq? node (session-current sess)) "✓" "✗")))
      (printf "无效节点编号。有效范围 1-~a\n"
              (length (session-collect-nodes sess)))))

;; ============================================================
;; 列出叶子
;; ============================================================

(define (show-leaves sess)
  (define leaves (history-leaves (session-history sess)))
  (printf "叶子节点 (共 ~a 个):\n" (length leaves))
  (for ([leaf (in-list leaves)] [i (in-naturals 1)])
    (define inp (history-node-input leaf))
    (define c (and inp (hash-ref inp 'content "")))
    (define mark (if (eq? leaf (session-current sess)) " ←" ""))
    (printf "  ~a. ~a~a\n" i
            (if c (if (> (string-length c) 50)
                      (string-append (substring c 0 50) "...")
                      c)
                "(root)")
            mark)))

;; ============================================================
;; 处理输入
;; ============================================================

(define (process-input sess line)
  (define parts (string-split line))
  (cond
    [(string=? line "/help")
     (show-help)
     sess]
    [(string=? line "/quit")
     (printf "再见。\n")
     (exit)]
    [(string=? line "/tree")
     (session-print-tree sess)
     sess]
    [(string=? line "/path")
     (session-print-path sess)
     sess]
    [(string=? line "/leaves")
     (show-leaves sess)
     sess]
    [(string=? line "/mark")
     (let*-values ([(s2 _) (session-organize sess)])
       s2)]
    [(string=? line "/organize")
     (let*-values ([(s2 r) (session-organize sess)])
       (when r
         (printf "\n📋 整理结果:\n~a\n" (response-content r)))
       s2)]
    [(string-prefix? line "/node ")
     (define n (string->number (cadr parts)))
     (show-node sess n)
     sess]
    [(string-prefix? line "/move ")
     (define n (string->number (cadr parts)))
     (define leaves (history-leaves (session-history sess)))
     (if (and n (<= 1 n (length leaves)))
         (let ([target (list-ref leaves (sub1 n))])
           (session-move sess target))
         (begin
           (display clr-red)
           (printf "无效的叶子编号。/leaves 查看可用编号。\n")
           (display format-reset)
           sess))]
    [(string=? line "/branch-same")
     (define inp (history-node-input (session-current sess)))
     (define input (and inp (hash-ref inp 'content "")))
     (if input
         (begin
           (printf "🌿 同输入分支: ~a\n\n" input)
           (let*-values ([(s2 r) (session-branch sess input
                                                  #:on-reasoning on-reasoning
                                                  #:on-content on-content
                                                  #:on-tool-calls on-tool-calls
                                                  #:tool-confirm tool-confirm
                                                  #:max-turns 10)])
             (printf "\n")
             s2))
         (begin
           (display clr-red)
           (printf "当前节点无输入，无法分支\n")
           (display format-reset)
           sess))]
    [(string-prefix? line "/branch ")
     (define input (string-join (cdr parts) " "))
     (printf "🌿 新输入分支: ~a\n\n" input)
     (let*-values ([(s2 r) (session-branch sess input
                                            #:on-reasoning on-reasoning
                                            #:on-content on-content
                                            #:on-tool-calls on-tool-calls
                                            #:tool-confirm tool-confirm
                                            #:max-turns 10)])
       (printf "\n")
       s2)]
    [(string-prefix? line "/")
     (display clr-red)
     (printf "未知命令: ~a\n" (car parts))
     (show-help)
     (display format-reset)
     sess]
    [else
     ;; 普通对话
     (printf "\n")
     (let*-values ([(s2 r) (session-chat sess line
                                          #:on-reasoning on-reasoning
                                          #:on-content on-content
                                          #:on-tool-calls on-tool-calls
                                          #:tool-confirm tool-confirm
                                          #:max-turns 10)])
       (printf "\n")
       s2)]))

;; ============================================================
;; 主循环
;; ============================================================

(define (main)
  (display clr-green)
  (display "=== 分支对话 REPL ===\n")
  (display format-reset)
  (show-help)
  (newline)

  (let loop ([sess (make-session env)])
    ;; 显示整理点状态
    (when (session-organize? sess)
      (display clr-yellow)
      (display "📌 ")
      (display format-reset))

    (display clr-cyan)
    (display "> ")
    (display format-reset)
    (flush-output)

    (define line (read-line))
    (cond
      [(eof-object? line)
       (printf "\n再见。\n")]
      [else
       (define new-sess (process-input sess (string-trim line)))
       (loop new-sess)])))

(main)
