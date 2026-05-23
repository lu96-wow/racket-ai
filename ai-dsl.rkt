#lang racket
;; ============================================================
;; ai-dsl.rkt — AI 对话 DSL
;;
;; 提供参数化环境，封装完整工具循环，支持 REPL 交互。
;; 支持对话历史的分支、跳转、删除。
;; ============================================================

(require "api-platform/deepseek/chat.rkt"
         "api-platform/deepseek/json-build-parse.rkt"
         "api-config/deepseek.rkt"
         "tools/deepseek-base-tool.rkt"
         "tools/tool.rkt"
         "format-color/core.rkt"
         "history/history.rkt")

;; 样式定义
(style-define! 'ai-prefix    (color-bg 4) (color-fg 15) attr-bold)
(style-define! 'tool-prefix  (color-bg 2) (color-fg 0) attr-bold)
(style-define! 'tool-name    (color-fg 10) attr-bold)
(style-define! 'prompt       (color-fg 6) attr-bold)
(style-define! 'reasoning    (color-fg 8) attr-italic)
(style-define! 'tool-result  (color-fg 8))
(style-define! 'max-turns    (color-fg 1) attr-bold)
(style-define! 'user-prefix  (color-bg 5) (color-fg 15) attr-bold)
(style-define! 'stream-hint  (color-fg 8) attr-dim)
(style-define! 'history-id   (color-fg 3) attr-bold)
(style-define! 'history-path (color-fg 6))

(provide
 ;; 参数
 current-messages current-tools current-model
 current-stream? current-tool-confirm-fn
 current-history-root current-history-node
 ;; 环境管理
 init-ai-env
 ;; 单次请求
 ai-str ai-stream
 ;; 工具循环
 ai-tool-loop
 ;; 提问宏/函数
 ai? ai-any
 ;; REPL
 enter-env quit-env
 ;; 命令处理（可在外部调用）
 ai-cmd
 ;; 确认开关
 tool-confirm-on tool-confirm-off
 tool-confirm-set tool-allow-set tool-confirm-clear
 ;; 历史控制
 history-tree history-path
 history-jump history-delete history-branch history-retry
 history-save history-load)

;; ============================================================
;; 参数
;; ============================================================

(define current-messages      (make-parameter '()))
(define current-tools         (make-parameter default-tools))
(define current-model         (make-parameter deepseek-v4-flash))
(define current-stream?       (make-parameter #t))

(define current-history-root (make-parameter (make-root)))
(define current-history-node (make-parameter (h-root-node (current-history-root))))

(define default-confirm-fn
  (lambda (name args)
    (display "  ")
    (display (format-styled 'tool-name (format "~a" name)))
    (display " 参数: ")
    (display (format-styled 'tool-result
                            (let ([args-str (format "~a" args)])
                              (if (> (string-length args-str) 80)
                                  (string-append (substring args-str 0 80) "...")
                                  args-str))))
    (printf "\n")
    (display (format-styled 'prompt "  确认执行? (y/n): "))
    (flush-output)
    (string-ci=? (read-line) "y")))

(define current-tool-confirm-fn (make-parameter default-confirm-fn))

;; ============================================================
;; 工具确认开关
;; ============================================================

(define (tool-confirm-on)
  (tool-set-global-confirm)
  (display (format-styled 'prompt "全局工具确认已开启\n")))

(define (tool-confirm-off)
  (tool-set-global-allow)
  (display (format-styled 'prompt "全局工具确认已关闭\n")))

(define (tool-confirm-set tool-name)
  (tool-set-confirm tool-name)
  (display (format-styled 'prompt (format "工具 ~a 确认已开启\n" tool-name))))

(define (tool-allow-set tool-name)
  (tool-set-allow tool-name)
  (display (format-styled 'prompt (format "工具 ~a 确认已关闭\n" tool-name))))

(define (tool-confirm-clear)
  (tool-clear-modes!)
  (display (format-styled 'prompt "所有工具级别确认设置已清除\n")))

;; ============================================================
;; 环境初始化
;; ============================================================

(define (init-ai-env)
  (current-messages '())
  (current-tools default-tools)
  (current-model deepseek-v4-flash)
  (current-stream? #t)
  (current-tool-confirm-fn default-confirm-fn)
  (tool-set-global-allow)
  (tool-clear-modes!)
  (current-history-root (make-root))
  (current-history-node (h-root-node (current-history-root)))
  (display (format-styled 'prompt "AI 环境已初始化。\n")))

;; ============================================================
;; 流中断回调
;; ============================================================

(define (stdin-stop?)
  (let ([stopped? #f])
    (lambda ()
      (cond
        [stopped? #t]
        [(char-ready? (current-input-port))
         (let drain ()
           (when (char-ready? (current-input-port))
             (read-char (current-input-port))
             (drain)))
         (set! stopped? #t)
         (display (format-styled 'max-turns "\n  [用户中断]\n"))
         (flush-output)
         #t]
        [else #f]))))

;; ============================================================
;; 历史函数（可外部调用）
;; ============================================================

(define (collect-subtree-nodes node)
  (cons node (append-map collect-subtree-nodes (h-node-children node))))

(define (history-tree)
  "显示历史树"
  (printf "  对话历史树:\n")
  (define root (current-history-root))
  (define current-node (current-history-node))
  (define all-leaves (h-leaves root))
  (for ([i (in-naturals 1)]
        [leaf (in-list all-leaves)])
    (define path (h-path leaf))
    (define is-current? (memq current-node path))
    (display (if is-current? "  * " "    "))
    (display (format-styled 'history-id (format "[~a] " i)))
    (for ([node (in-list (cdr path))])
      (define u (h-node-user-content node))
      (define short-u (if (> (string-length u) 30)
                          (string-append (substring u 0 30) "...")
                          u))
      (display (format-styled 'history-path (format "→ ~a " short-u))))
    (when is-current? (display (format-styled 'prompt " ◀ 当前")))
    (printf "\n")))

(define (history-path)
  "显示当前路径"
  (define node (current-history-node))
  (define path (h-path node))
  (display "  当前路径: ")
  (for ([n (in-list (cdr path))])
    (define u (h-node-user-content n))
    (define short-u (if (> (string-length u) 40)
                        (string-append (substring u 0 40) "...")
                        u))
    (display (format-styled 'history-path (format "→ ~a " short-u))))
  (printf "\n"))

(define (history-jump index)
  "跳转到指定叶子"
  (define root (current-history-root))
  (define leaves (h-leaves root))
  (if (or (< index 1) (> index (length leaves)))
      (display (format-styled 'max-turns (format "  无效索引: ~a (共 ~a 个叶子)\n" index (length leaves))))
      (let ([target (list-ref leaves (sub1 index))])
        (current-history-node target)
        (define path (h-path target))
        (define msgs '())
        (for ([node (in-list (cdr path))])
          (define u (h-node-user-content node))
          (define a (h-node-ai-content node))
          (set! msgs (append msgs
                             (build-messages (build-user-message #:content u)
                                             (build-assistant-message #:content (if (string? a) a ""))))))
        (current-messages msgs)
        (display (format-styled 'prompt (format "  已跳转到 [~a]\n" index)))
        (history-path))))

(define (history-delete index)
  "删除指定叶子"
  (define root (current-history-root))
  (define leaves (h-leaves root))
  (if (or (< index 1) (> index (length leaves)))
      (display (format-styled 'max-turns (format "  无效索引: ~a\n" index)))
      (let ([target (list-ref leaves (sub1 index))])
        (h-delete! root target)
        (when (memq (current-history-node) (collect-subtree-nodes target))
          (current-history-node (h-root-node root))
          (current-messages '()))
        (display (format-styled 'prompt (format "  已删除 [~a]\n" index))))))

(define (history-branch)
  "在当前节点分叉"
  (define root (current-history-root))
  (define node (current-history-node))
  (if (not (h-node-parent node))
      (display (format-styled 'max-turns "  不能从根节点分叉\n"))
      (let ([new-node (h-branch-same root node)])
        (current-history-node new-node)
        (define path (h-path new-node))
        (define msgs '())
        (for ([n (in-list (cdr (drop-right path 1)))])
          (define u (h-node-user-content n))
          (define a (h-node-ai-content n))
          (set! msgs (append msgs
                             (build-messages (build-user-message #:content u)
                                             (build-assistant-message #:content (if (string? a) a ""))))))
        (define last-u (h-node-user-content new-node))
        (set! msgs (append msgs (build-messages (build-user-message #:content last-u))))
        (current-messages msgs)
        (display (format-styled 'prompt "  已创建分叉\n"))
        (history-path))))

(define (history-retry)
  "重新生成当前回答"
  (define root (current-history-root))
  (define node (current-history-node))
  (if (not (h-node-parent node))
      (display (format-styled 'max-turns "  不能从根节点重试\n"))
      (let ([new-node (h-branch-same root node)])
        (current-history-node new-node)
        (define path (h-path new-node))
        (define msgs '())
        (for ([n (in-list (cdr (drop-right path 1)))])
          (define u (h-node-user-content n))
          (define a (h-node-ai-content n))
          (set! msgs (append msgs
                             (build-messages (build-user-message #:content u)
                                             (build-assistant-message #:content (if (string? a) a ""))))))
        (define last-u (h-node-user-content new-node))
        (current-messages msgs)
        (display (format-styled 'prompt "  重新回答: "))
        (display (format-styled 'user-prefix " user "))
        (display (format-styled 'stream-hint last-u))
        (printf "\n")
        (ai-tool-loop last-u))))

(define (history-save path)
  (h-save (current-history-root) path)
  (display (format-styled 'prompt (format "  历史已保存到 ~a\n" path))))

(define (history-load path)
  (define root (h-load path))
  (current-history-root root)
  (current-history-node (car (h-leaves root)))
  (current-messages '())
  (display (format-styled 'prompt (format "  历史已从 ~a 加载\n" path))))

;; ============================================================
;; 历史消息
;; ============================================================

(define (history-show-messages)
  "显示当前消息历史"
  (printf "  对话历史:\n")
  (for ([msg (in-list (current-messages))])
    (define role (hash-ref msg 'role))
    (define content (hash-ref msg 'content #f))
    (cond
      [(string=? role "user")
       (printf "    user: %s\n" content)]
      [(string=? role "assistant")
       (define short-content (if (and content (> (string-length content) 80))
                                 (string-append (substring content 0 80) "...")
                                 content))
       (printf "    ai: %s\n" short-content)]
      [(string=? role "tool")
       (printf "    [tool]\n")])))

;; ============================================================
;; 单次请求/响应
;; ============================================================

(define (ai-stream prompt
                   #:on-content [on-content void]
                   #:on-reasoning [on-reasoning void]
                   #:on-tool-calls [on-tool-calls void])
  (define msgs (current-messages))
  (define tools (current-tools))
  (define model (current-model))
  (define messages (append msgs (build-messages (build-user-message #:content prompt))))
  (define req (build-chat-request #:model model #:messages messages
                                  #:stream #t
                                  #:max_tokens 8192
                                  #:tools (tools-schemas tools)))
  (define acc-cc "")
  (define acc-rc "")
  (define tc-hash (hasheq))
  (define stop? (stdin-stop?))

  (deepseek-chat/stream req
                        #:stop? stop?
                        (cons 'content (lambda (c)
                                         (set! acc-cc (string-append acc-cc c))
                                         (on-content c)))
                        (cons 'reasoning (lambda (r)
                                           (set! acc-rc (string-append acc-rc r))
                                           (on-reasoning r)))
                        (cons 'tool-calls (lambda (tcs)
                                            (set! tc-hash (merge-tool-calls tc-hash tcs))
                                            (on-tool-calls tcs))))

  (define sorted-keys (sort (hash-keys tc-hash) <))
  (define tool-calls (for/list ([k (in-list sorted-keys)]) (hash-ref tc-hash k)))
  (hasheq 'content acc-cc 'reasoning acc-rc 'tool-calls tool-calls))

(define (ai-str prompt)
  (display (format-styled 'stream-hint "(按 Enter 中断) "))
  (flush-output)
  (ai-stream prompt
             #:on-content (lambda (c) (display c) (flush-output))
             #:on-reasoning (lambda (r) (display (format-styled 'reasoning r)) (flush-output))
             #:on-tool-calls (lambda (tcs) (void))))

(define-syntax ai-any
  (lambda (stx)
    (syntax-case stx ()
      [(_ expr ...)
       #'(let ([parts (list (lambda () (format "~a" expr)) ...)])
           (define str (string-join (map (lambda (f) (f)) parts) " "))
           (ai-tool-loop str))])))

;; ============================================================
;; 工具循环（修改后）
;; ============================================================

(define (ai-tool-loop prompt
                      #:max-turns [max-turns 10]
                      #:tool-confirm [tool-confirm (current-tool-confirm-fn)]
                      #:on-content [on-content (lambda (c) (display c) (flush-output))]
                      #:on-reasoning [on-reasoning (lambda (r)
                                                     (display (format-styled 'reasoning r))
                                                     (flush-output))]
                      #:on-tool-call [on-tool-call (lambda (name args result) (void))])
  (let* ([msgs (current-messages)]
         [tools (current-tools)]
         [model (current-model)]
         [cf (or tool-confirm (current-tool-confirm-fn))]
         [root (current-history-root)]
         [node (current-history-node)])

    (display (format-styled 'user-prefix " user "))
    (display (format-styled 'stream-hint prompt))
    (printf "\n")

    ;; === 历史树更新（只记录用户输入） ===
    (define-values (new-root child)
      (if (h-node-user-content node)
          (h-branch-new root node prompt)
          (h-next root node prompt)))
    (set! root new-root)
    (current-history-root root)
    (current-history-node child)
    (define current-child child)

    ;; === 工作消息：仅用于工具循环，工具历史会丢弃 ===
    (let loop ([work-messages (append msgs (build-messages (build-user-message #:content prompt)))]
               [turn 0])
      (if (>= turn max-turns)
          (display (format-styled 'max-turns "[达到最大轮数]\n"))
          (let* ([req (build-chat-request #:model model #:messages work-messages
                                          #:stream #t #:thinking #t
                                          #:max_tokens 8192
                                          #:tools (tools-schemas tools))]
                 [acc-cc ""] [acc-rc ""] [tc-hash (hasheq)]
                 [reasoning-started #f] [content-started #f]
                 [stop? (stdin-stop?)])

            (display (format-styled 'ai-prefix " AI "))
            (display (format-styled 'stream-hint "(按 Enter 中断) "))
            (flush-output)

            (deepseek-chat/stream req
                                  #:stop? stop?
                                  (cons 'content (lambda (c)
                                                   (set! acc-cc (string-append acc-cc c))
                                                   (unless content-started
                                                     (set! content-started #t)
                                                     (when reasoning-started
                                                       (printf "\n")
                                                       (display (format-styled 'ai-prefix " AI "))
                                                       (flush-output)))
                                                   (on-content c)))
                                  (cons 'reasoning (lambda (r)
                                                     (set! acc-rc (string-append acc-rc r))
                                                     (unless reasoning-started
                                                       (set! reasoning-started #t)
                                                       (unless content-started
                                                         (display (format-styled 'reasoning "[思考] ")))
                                                       (flush-output))
                                                     (on-reasoning r)))
                                  (cons 'tool-calls (lambda (tcs)
                                                      (set! tc-hash (merge-tool-calls tc-hash tcs)))))
            (printf "\n")

            (let* ([sorted-keys (sort (hash-keys tc-hash) <)]
                   [tool-calls (for/list ([k (in-list sorted-keys)]) (hash-ref tc-hash k))]
                   [asst-msg (if (null? tool-calls)
                                 (build-assistant-message #:content acc-cc #:reasoning_content acc-rc)
                                 (build-assistant-message #:content acc-cc #:reasoning_content acc-rc
                                                          #:tool_calls tool-calls))])

              ;; === 判断是否为最终回复（无工具调用） ===
              (if (null? tool-calls)
                  ;; === 最终回复：只保存净输入输出到全局历史 ===
                  (let ([clean-messages
                         (append msgs
                                 (build-messages
                                  (build-user-message #:content prompt)
                                  (build-assistant-message #:content acc-cc #:reasoning_content acc-rc)))])
                    (current-messages clean-messages)
                    (h-set-ai! root current-child acc-cc))

                  ;; === 工具调用：仅在 work-messages 中保存，循环结束后丢弃 ===
                  (let ()
                    (set! work-messages (append work-messages (build-messages asst-msg)))
                    (display (format-styled 'tool-prefix (format " 工具 x~a " (length tool-calls))))
                    (printf "\n")

                    (define all-confirmed?
                      (for/and ([tc (in-list tool-calls)])
                        (let* ([id (tool-call-id tc)]
                               [name (tool-call-func-name tc)]
                               [args (tool-call-func-args tc)]
                               [args-str (format "~a" args)])

                          (display "  ")
                          (display (format-styled 'tool-name (format "~a" name)))
                          (display " 参数: ")
                          (display (format-styled 'tool-result
                                                  (if (> (string-length args-str) 80)
                                                      (string-append (substring args-str 0 80) "...")
                                                      args-str)))
                          (printf "\n")

                          (define result (tool-dispatch tools name args-str cf))
                          (cond
                            [(or (string-prefix? result "[安全拦截]")
                                 (string-prefix? result "[取消]"))
                             (display (format-styled 'max-turns
                                                     (format "  ✗ 已拒绝: ~a\n"
                                                             (if (string-prefix? result "[安全拦截]") "安全拦截" "用户取消"))))
                             ;; 工具被拒绝：也在 work-messages 中记录，但继续循环
                             (set! work-messages (append work-messages
                                                         (build-messages (build-tool-result
                                                                          #:tool_call_id id
                                                                          #:content "操作已被用户拒绝，请勿重试。继续对话。"))))
                             #f]
                            [(string-prefix? result "[工具错误]")
                             (display (format-styled 'max-turns (format "  ✗ ~a\n" result)))
                             (set! work-messages (append work-messages
                                                         (build-messages (build-tool-result
                                                                          #:tool_call_id id
                                                                          #:content (format "工具执行失败: ~a，请尝试其他方法。" result)))))
                             #f]
                            [else
                             (display (format-styled 'prompt "  ✓ 已执行\n"))
                             (on-tool-call name args result)
                             (define short-result (if (> (string-length result) 100)
                                                      (substring result 0 100) result))
                             (display "    -> ")
                             (display (format-styled 'tool-result (string-replace short-result "\n" " ")))
                             (when (> (string-length result) 100)
                               (display (format-styled 'tool-result "...")))
                             (printf "\n")
                             ;; 工具成功：在 work_messages 中添加结果
                             (set! work-messages (append work-messages
                                                         (build-messages (build-tool-result
                                                                          #:tool_call_id id #:content result))))
                             #t]))))

                    (printf "\n")
                    ;; 继续循环（work-messages 会越来越大，但不会被外部看到）
                    (when all-confirmed?
                      (loop work-messages (add1 turn)))))))))))

;; ai? 宏
(define-syntax-rule (ai? arg ...)
  (ai-tool-loop (string-append (format "~a" (quote arg)) ...)))

;; ============================================================
;; 调试：打印实际构建的消息历史
;; ============================================================

(define (history-show-raw)
  "显示当前消息历史的原始结构（实际发送给API的格式）"
  (printf "  当前消息历史 (共 ~a 条消息):\n" (length (current-messages)))
  (for ([msg (in-list (current-messages))]
        [i (in-naturals 1)])
    (printf "  [~a] " i)
    (pretty-print msg)
    (printf "\n")))

(define (history-show-compact)
  "显示消息历史的紧凑格式（角色+内容摘要）"
  (printf "  消息序列:\n")
  (for ([msg (in-list (current-messages))]
        [i (in-naturals 1)])
    (define role (hash-ref msg 'role))
    (define content (hash-ref msg 'content #f))
    (define tc (hash-ref msg 'tool_calls #f))
    (define tool-call-id (hash-ref msg 'tool_call_id #f))

    (printf "  [~a] " i)
    (display (format-styled 'tool-name (format "~a" role)))

    (cond
      [tc
       (printf " (工具调用 x~a): " (length tc))
       (for ([t (in-list tc)])
         (printf "~a " (tool-call-func-name t)))]
      [tool-call-id
       (printf " (结果): ")
       (define short (if (> (string-length content) 60)
                         (string-append (substring content 0 60) "...")
                         content))
       (printf "~a" short)]
      [content
       (printf ": ")
       (define short (if (> (string-length content) 80)
                         (string-append (substring content 0 80) "...")
                         content))
       (printf "~a" short)])
    (printf "\n")))

(define (history-show-json)
  "以格式化的JSON显示消息历史"
  (define msgs (current-messages))
  ;; 先构建完整的messages数组jsexpr
  (define messages-jsexpr
    (hasheq 'messages msgs))

  (printf "  消息历史 (JSON格式):\n")
  (pretty-print messages-jsexpr))

;; ============================================================
;; 带请求完整信息的调试输出
;; ============================================================

(define (history-show-debug)
  "完整调试信息：消息序列 + API请求结构"
  (printf "══════════════════════════════════════════\n")
  (printf "  API 请求构建预览\n")
  (printf "══════════════════════════════════════════\n")

  ;; 显示每个消息的详细结构
  (printf "\n  消息序列 (%d条):\n" (length (current-messages)))
  (printf "  ───────────────────────────────────────\n")
  (for ([msg (in-list (current-messages))]
        [i (in-naturals 1)])
    (printf "  [消息 %d]\n" i)
    (pretty-print msg)
    (printf "  ───────────────────────────────────────\n"))

  ;; 显示完整的请求结构
  (printf "\n  完整请求体:\n")
  (printf "  ───────────────────────────────────────\n")
  (define req
    (build-chat-request
     #:model (current-model)
     #:messages (current-messages)
     #:stream #t
     #:max_tokens 8192
     #:tools (tools-schemas (current-tools))))
  (pretty-print req)
  (printf "══════════════════════════════════════════\n"))

;; ============================================================
;; 命令处理函数（可在外部调用）
;; ============================================================

(define (ai-cmd line)
  "处理一行命令/对话输入。返回 #t 继续，返回 #f 退出。"
  (cond
    [(eof-object? line) (printf "\n") #t]
    [(string=? line ":quit") (quit-env) #f]

    ;; 历史命令
    [(string=? line ":tree") (history-tree) #t]
    [(string=? line ":path") (history-path) #t]
    [(string=? line ":history") (history-show-raw) #t]      ; 改为原始格式
    [(string=? line ":h") (history-show-compact) #t]         ; 紧凑格式
    [(string=? line ":hjson") (history-show-json) #t]        ; JSON格式
    [(string=? line ":hdebug") (history-show-debug) #t]      ; 完整调试
    [(string-prefix? line ":jump ")
     (define idx (string->number (substring line 6)))
     (if idx (history-jump idx) (display "  需要数字索引\n"))
     #t]
    [(string-prefix? line ":del ")
     (define idx (string->number (substring line 5)))
     (if idx (history-delete idx) (display "  需要数字索引\n"))
     #t]
    [(string=? line ":branch") (history-branch) #t]
    [(string=? line ":retry") (history-retry) #t]
    [(string-prefix? line ":save ")
     (history-save (substring line 6)) #t]
    [(string-prefix? line ":load ")
     (history-load (substring line 6)) #t]

    ;; 工具确认命令
    [(string=? line ":confirm-on") (tool-confirm-on) #t]
    [(string=? line ":confirm-off") (tool-confirm-off) #t]
    [(string-prefix? line ":confirm ")
     (tool-confirm-set (substring line 9)) #t]
    [(string-prefix? line ":allow ")
     (tool-allow-set (substring line 7)) #t]
    [(string=? line ":confirm-clear") (tool-confirm-clear) #t]

    ;; 其他命令
    [(string=? line ":history") (history-show-messages) #t]
    [(string=? line ":tools")
     (display (format-styled 'prompt "  当前工具: "))
     (displayln (map car (tools-names (current-tools))))
     #t]
    [(string=? line ":clear")
     (current-messages '())
     (display (format-styled 'prompt "  历史已清空\n"))
     #t]

    ;; 对话输入
    [else (ai-tool-loop line) #t]))

;; ============================================================
;; REPL
;; ============================================================

(define (enter-env)
  (display (format-styled 'prompt "\n输入 :quit 退出，直接输入问题进行对话。\n"))
  (display (format-styled 'stream-hint "命令: :tree :jump N :del N :branch :retry :path :save :load\n"))
  (let repl-loop ()
    (display (format-styled 'prompt "\nai> "))
    (flush-output)
    (define line (read-line))
    (define continue? (ai-cmd line))
    (when continue? (repl-loop))))

(define (quit-env)
  (display (format-styled 'prompt "\nAI 会话结束。\n")))