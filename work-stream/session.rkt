#lang racket
;; ============================================================
;; work-stream/session.rkt — 分支对话流
;;
;; 基于 history 树，流式输出，工具自动循环，整理点机制。
;;
;; 核心改动（相对 v1）：
;;   - 使用 env-chat/stream 替代 env-chat
;;   - do-chat 内部处理工具调用循环
;;   - history-node-output 存储完整消息列表（含 tool 中间消息）
;;   - 非阻塞 \n 中断（make-newline-stop?）
;; ============================================================

(require "../history/history.rkt"
         "../env/deepseek-env.rkt"
         "../api-platform/deepseek/json-build-parse.rkt"
         "../api-config/deepseek.rkt"
         "../tools/deepseek-base-tool.rkt"
         "input.rkt")

(provide
 make-session session?
 session-history session-current session-env session-organize-node
 session-chat session-branch session-organize session-organize?
 session-move session-path-messages
 session-print-path session-print-tree
 session-collect-nodes session-find-node)

;; ============================================================
;; Session 结构
;; ============================================================

(struct session (history       ; history-root
                 current       ; history-node
                 env           ; chat environment
                 organize-node) ; #f | history-node
  #:transparent)

;; ============================================================
;; 构造
;; ============================================================

(define (make-session env)
  (define h (history-make-root))
  (session h (history-root-node h) env #f))

;; ============================================================
;; 内部：从路径提取消息列表
;; ============================================================

(define (session-path-messages sess)
  (define path (history-path (session-current sess)))
  (define nodes (cdr path))
  (apply append
         (for/list ([n nodes])
           (define inp (history-node-input n))
           (define out-msgs (history-node-output n))
           (append (if inp (list inp) '())
                   (if out-msgs out-msgs '())))))

;; ============================================================
;; 内部：构建整理提示词
;; ============================================================

(define (build-organize-prompt nodes)
  (define text
    (string-join
     (for/list ([n nodes])
       (define inp (history-node-input n))
       (define out-msgs (history-node-output n))
       (string-append
        (if inp (format "用户: ~a\n" (hash-ref inp 'content "")) "")
        (if out-msgs
            (string-join
             (for/list ([m out-msgs])
               (format "  [~a]: ~a\n" (hash-ref m 'role) (hash-ref m 'content "")))
             "")
            "")))
     "\n"))
  (format
   "以下是某段探索过程的对话记录。请提炼核心认知：\n\
    - 哪些是已验证的、模型自身不具备的知识？\n\
    - 哪些是错误尝试和过程噪音（可以丢弃）？\n\
    - 哪些是需要记住的关键发现？\n\n\
    对话记录：\n~a\n\n\
    请只输出提炼后的认知精华，不要保留原始对话。" text))

;; ============================================================
;; 内部：do-chat — 流式对话 + 工具循环
;; ============================================================

(define (do-chat env initial-msgs
                 #:on-content [on-content #f]
                 #:on-reasoning [on-reasoning #f]
                 #:on-tool-calls [on-tool-calls #f]
                 #:stop? [stop? (make-newline-stop?)]
                 #:max-turns [max-turns 10]
                 #:tool-confirm [tool-confirm #f]
                 #:override [override #f])
  (let loop ([msgs initial-msgs] [turns 0])
    (define resp (env-chat/stream env msgs
                                  #:on-content on-content
                                  #:on-reasoning on-reasoning
                                  #:on-tool-calls on-tool-calls
                                  #:stop? stop?
                                  #:override override))
    (define tcs (response-tool-calls resp))
    (cond
      [(not tcs)
       ;; AI 不再调用工具，正常返回
       (values resp msgs)]
      [(>= turns max-turns)
       ;; 超过最大轮数，自动取消剩余工具
       (define tools (env-tools env))
       (if (not tools)
           (values resp msgs)
           (let ([cancel-msgs (tool-cancel-chat-request tools resp msgs
                               #:cancel-message "工具调用次数超限，自动取消")])
             (values resp cancel-msgs)))]
      [else
       (define tools (env-tools env))
       (unless tools
         (error 'do-chat "tool calls but no tools configured in env"))
       ;; 从完整 resp 中提取工具信息，不是从流式片段
       (define should-execute
         (if tool-confirm
             (for/and ([tc (in-list tcs)])
               (define name (tool-call-func-name tc))
               (define args (tool-call-func-args tc))
               (tool-confirm name args))
             #t))
       (if should-execute
           ;; 用户确认，执行工具
           (let ([new-msgs (tool-chat-request tools resp msgs)])
             (loop new-msgs (add1 turns)))
           ;; 用户取消，生成终止消息
           (let ([cancel-msgs (tool-cancel-chat-request tools resp msgs)])
             (values resp cancel-msgs)))])))

;; ============================================================
;; session-chat : 正常对话（流式 + 工具循环）
;; ============================================================

(define (session-chat sess input-str
                      #:on-content [on-content #f]
                      #:on-reasoning [on-reasoning #f]
                      #:on-tool-calls [on-tool-calls #f]
                      #:stop? [stop? (make-newline-stop?)]
                      #:max-turns [max-turns 10]
                      #:tool-confirm [tool-confirm #f]
                      #:override [override #f])
  (define env (session-env sess))
  (define input-msg
    (car (build-messages (build-user-message #:content input-str))))
  (define initial-msgs
    (append (session-path-messages sess) (list input-msg)))
  (let*-values ([(resp full-msgs)
                 (do-chat env initial-msgs
                          #:on-content on-content
                          #:on-reasoning on-reasoning
                          #:on-tool-calls on-tool-calls
                          #:stop? stop?
                          #:max-turns max-turns
                          #:tool-confirm tool-confirm
                          #:override override)])
    (define new-msgs (list-tail full-msgs (length initial-msgs)))
    (let*-values ([(h new-node)
                   (history-next (session-history sess)
                                 (session-current sess) input-msg)])
      (set-history-node-output! new-node new-msgs)
      (define org-node (session-organize-node sess))
      (define new-org
        (if (and org-node (memq org-node (history-path new-node)))
            org-node #f))
      (values (session h new-node env new-org) resp))))

;; ============================================================
;; session-branch : 从当前节点分叉
;; ============================================================

(define (session-branch sess input-str
                        #:on-content [on-content #f]
                        #:on-reasoning [on-reasoning #f]
                        #:on-tool-calls [on-tool-calls #f]
                        #:stop? [stop? (make-newline-stop?)]
                        #:max-turns [max-turns 10]
                        #:tool-confirm [tool-confirm #f]
                        #:override [override #f])
  (define env (session-env sess))
  (define input-msg
    (car (build-messages (build-user-message #:content input-str))))
  (define initial-msgs
    (append (session-path-messages sess) (list input-msg)))
  (let*-values ([(resp full-msgs)
                 (do-chat env initial-msgs
                          #:on-content on-content
                          #:on-reasoning on-reasoning
                          #:on-tool-calls on-tool-calls
                          #:stop? stop?
                          #:max-turns max-turns
                          #:tool-confirm tool-confirm
                          #:override override)])
    (define new-msgs (list-tail full-msgs (length initial-msgs)))
    (let*-values ([(h new-node)
                   (history-branch-new (session-history sess)
                                       (session-current sess) input-msg)])
      (set-history-node-output! new-node new-msgs)
      (values (session h new-node env (session-organize-node sess)) resp))))


;; ============================================================
;; session-organize : 整理点管理
;;   第一次调用 → 标记当前节点
;;   第二次调用 → 从整理点开始执行 AI 压缩 → 新分支
;; ============================================================

(define (session-organize sess)
  (define org-node (session-organize-node sess))
  (cond
    [(not org-node)
     (printf "📌 标记整理点\n")
     (values (session (session-history sess)
                      (session-current sess)
                      (session-env sess)
                      (session-current sess))
             #f)]
    [else
     (printf "🔄 执行整理...\n")
     (let* ([current (session-current sess)]
            [full-path (history-path current)]
            [path-from-org (drop-while
                            (lambda (n) (not (eq? n org-node)))
                            full-path)])
       (unless (pair? path-from-org)
         (error 'session-organize "organize node not on path to current"))
       (let* ([nodes-to-summarize path-from-org]
              [prompt (build-organize-prompt nodes-to-summarize)]
              [env (session-env sess)]
              [resp (env-chat env prompt)]
              [summary (or (response-content resp) "")]
              [summary-msg
               (car (build-messages
                     (build-user-message
                      #:content
                      (string-append "【整理结果】\n" summary))))])
         (let*-values ([(h new-node)
                        (history-branch-new (session-history sess)
                                            org-node summary-msg)])
           (set-history-node-output! new-node
             (list (hasheq 'role "assistant"
                           'content (or (response-content resp) ""))))
           (printf "✅ 整理完成，已创建新分支\n")
           (printf "   压缩 ~a 条消息为认知摘要\n" (length nodes-to-summarize))
           (values (session h new-node env #f) resp))))]))

;; ============================================================
;; session-move : 移动到叶子节点
;; ============================================================

(define (session-move sess target-node)
  (unless (memq target-node (history-leaves (session-history sess)))
    (error 'session-move "target node is not a leaf"))
  (define org-node (session-organize-node sess))
  (define new-org
    (if (and org-node (memq org-node (history-path target-node)))
        org-node
        (begin
          (when org-node
            (printf "📌 整理点已不在当前路径，已清空\n"))
          #f)))
  (session (session-history sess) target-node
           (session-env sess) new-org))

;; ============================================================
;; session-organize? : 检查整理点
;; ============================================================

(define (session-organize? sess)
  (and (session-organize-node sess) #t))

;; ============================================================
;; 辅助
;; ============================================================

(define (drop-while pred lst)
  (cond [(null? lst) '()]
        [(pred (car lst)) (drop-while pred (cdr lst))]
        [else lst]))

;; ============================================================
;; 打印工具
;; ============================================================

(define (session-print-path sess)
  (define path (history-path (session-current sess)))
  (printf "路径 (共 ~a 节点):\n" (length path))
  (for ([n path] [i (in-naturals)])
    (define inp (history-node-input n))
    (define label
      (cond [(eq? n (session-organize-node sess)) " 📌"]
            [(eq? n (session-current sess)) " ←"]
            [else ""]))
    (define prefix
      (if (eq? n (history-root-node (session-history sess)))
          "[root]"
          (let ([c (and inp (hash-ref inp 'content ""))])
            (if (and c (> (string-length c) 40))
                (string-append (substring c 0 40) "...")
                (or c "")))))
    (printf "  ~a. ~a~a\n" i prefix label)))

(define (session-collect-nodes sess)
  "返回所有非 root 节点的列表（深度优先）"
  (define root (history-root-node (session-history sess)))
  (define result '())
  (define (walk n)
    (for ([c (in-list (history-node-children n))])
      (set! result (append result (list c)))
      (walk c)))
  (walk root)
  result)

(define (session-find-node sess idx)
  "按序号（1-based）查找节点，序号来自 session-print-tree 显示的数字"
  (define nodes (session-collect-nodes sess))
  (and (>= idx 1) (<= idx (length nodes))
       (list-ref nodes (sub1 idx))))

(define (session-print-tree sess)
  (define nodes (session-collect-nodes sess))
  (define root (history-root-node (session-history sess)))
  (define node->idx
    (for/hash ([n (in-list nodes)] [i (in-naturals 1)])
      (values n i)))
  (define (print-node n depth)
    (define inp (history-node-input n))
    (define idx-str
      (if (eq? n root)
          ""
          (format "[~a] " (hash-ref node->idx n ""))))
    (define label
      (cond [(eq? n (session-current sess)) " ←"]
            [(eq? n (session-organize-node sess)) " 📌"]
            [else ""]))
    (define prefix
      (if (eq? n root)
          "root"
          (let ([c (and inp (hash-ref inp 'content ""))])
            (if (and c (> (string-length c) 30))
                (string-append (substring c 0 30) "...")
                (or c "")))))
    (printf "~a~a~a~a\n" (make-string (* depth 2) #\space) idx-str prefix label)
    (for ([c (in-list (history-node-children n))])
      (print-node c (add1 depth))))
  (printf "--- 树 (共 ~a 节点) ---\n" (add1 (length nodes)))
  (print-node root 0))
