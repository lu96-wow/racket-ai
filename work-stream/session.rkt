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
 session-print-path session-print-tree)

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
                 #:override [override #f])
  (let loop ([msgs initial-msgs])
    (define resp (env-chat/stream env msgs
                                  #:on-content on-content
                                  #:on-reasoning on-reasoning
                                  #:on-tool-calls on-tool-calls
                                  #:stop? stop?
                                  #:override override))
    (define tcs (response-tool-calls resp))
    (if (not tcs)
        (values resp msgs)
        (let ([tools (env-tools env)])
          (unless tools
            (error 'do-chat "tool calls but no tools configured in env"))
          (define new-msgs (tool-chat-request tools resp msgs))
          (loop new-msgs)))))

;; ============================================================
;; session-chat : 正常对话（流式 + 工具循环）
;; ============================================================

(define (session-chat sess input-str
                      #:on-content [on-content #f]
                      #:on-reasoning [on-reasoning #f]
                      #:on-tool-calls [on-tool-calls #f]
                      #:stop? [stop? (make-newline-stop?)]
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

(define (session-print-tree sess)
  (define (print-node n depth)
    (define inp (history-node-input n))
    (define label
      (cond [(eq? n (session-current sess)) " ←"]
            [(eq? n (session-organize-node sess)) " 📌"]
            [else ""]))
    (define prefix
      (if (eq? n (history-root-node (session-history sess)))
          "root"
          (let ([c (and inp (hash-ref inp 'content ""))])
            (if (and c (> (string-length c) 30))
                (string-append (substring c 0 30) "...")
                (or c "")))))
    (printf "~a~a~a\n" (make-string (* depth 2) #\space) prefix label)
    (for ([c (in-list (history-node-children n))])
      (print-node c (add1 depth))))
  (print-node (history-root-node (session-history sess)) 0))
