#lang racket
;; ============================================================
;; tools/tool.rkt — 工具抽象框架
;;
;; 提供工具定义、工具集操作、分发调度、模式控制。
;; 不包含具体工具（具体工具在 base-tool.rkt）。
;; ============================================================

(require json)

(provide
 make-tool build-tools tools-add tools-delete tools-schemas tools-names
 tool-dispatch
 tool-set-global-allow tool-set-global-confirm tool-global-mode
 tool-set-allow tool-set-confirm tool-clear-modes!
 merge-tool-calls)

(struct tool (name schema run security) #:prefab)
(struct tools (schema-alist run-table security-table) #:prefab)

(define (make-tool name schema #:run run #:security [sec #f])
  (tool name schema run sec))

(define (build-tools . ts)
  (tools (for/list ([t ts]) (cons (tool-name t) (tool-schema t)))
         (for/hash ([t ts]) (values (tool-name t) (tool-run t)))
         (for/hash ([t ts]) (values (tool-name t) (tool-security t)))))

(define (tools-add ts . ts2)
  (tools (append (tools-schema-alist ts)
                 (for/list ([t ts2]) (cons (tool-name t) (tool-schema t))))
         (for/fold ([h (tools-run-table ts)]) ([t ts2])
           (hash-set h (tool-name t) (tool-run t)))
         (for/fold ([h (tools-security-table ts)]) ([t ts2])
           (hash-set h (tool-name t) (tool-security t)))))

(define (tools-delete ts . names)
  (define ns (list->set names))
  (tools (filter (lambda (p) (not (set-member? ns (car p)))) (tools-schema-alist ts))
         (for/fold ([h (hash)]) ([(k v) (in-hash (tools-run-table ts))]
                                 #:unless (set-member? ns k)) (hash-set h k v))
         (for/fold ([h (hash)]) ([(k v) (in-hash (tools-security-table ts))]
                                 #:unless (set-member? ns k)) (hash-set h k v))))

(define (tools-schemas ts) (map cdr (tools-schema-alist ts)))
(define (tools-names ts) (map car (tools-schema-alist ts)))

;; ============================================================
;; 模式控制
;; ============================================================

(define tool-global-mode (box #f))           ;; #f = allow, #t = confirm
(define tool-per-modes   (make-hash))         ;; name -> #t (confirm)
(define (tool-set-global-allow)   (set-box! tool-global-mode #f))
(define (tool-set-global-confirm) (set-box! tool-global-mode #t))


(define (tool-set-allow name)   (hash-remove! tool-per-modes name))
(define (tool-set-confirm name) (hash-set! tool-per-modes name #t))
(define (tool-clear-modes!)     (hash-clear! tool-per-modes))

(define (need-confirm? name)
  (or (unbox tool-global-mode)
      (hash-ref tool-per-modes name #f)))

;; ============================================================
;; 分发
;; ============================================================

(define (parse-args-json json)
  (with-handlers ([exn:fail? (lambda (e) (cons 'error (exn-message e)))])
    (cons 'ok (if (or (equal? json "{}") (equal? json "")) (hasheq) (string->jsexpr json)))))

(define (try-execute run name args)
  (with-handlers ([exn:fail? (lambda (e) (format "[工具错误] ~a: ~a" name (exn-message e)))])
    (run args)))

(define (tool-dispatch ts name json (confirm-fn (lambda (n a) #t)))
  (match (parse-args-json json)
    [(cons 'error msg) (format "tool argument parse error (~a): ~a" name msg)]
    [(cons 'ok args)
     (define run (hash-ref (tools-run-table ts) name #f))
     (cond
       [(not run) (format "unknown tool: ~a" name)]
       [else
        (define sec (hash-ref (tools-security-table ts) name #f))
        (define sec-err (and sec (sec args)))
        (if sec-err
            ;; 安全拦截：始终调用确认回调，让用户选择是否强制执行
            (if (confirm-fn name args)
                ;; 用户确认风险，强制执行
                (try-execute run name args)
                ;; 用户拒绝，返回拦截信息
                (format "[安全拦截] ~a: ~a" name sec-err))
            ;; 无安全拦截：正常流程
            (let ([need? (need-confirm? name)])
              (if need?
                  (if (confirm-fn name args)
                      (try-execute run name args)
                      (format "[取消] ~a: 用户取消了操作" name))
                  (try-execute run name args))))])]))

;; ============================================================
;; 流式合并
;; ============================================================

(define (deep-merge b o)
  (for/fold ([m b]) ([(k v) (in-hash o)])
    (cond [(and (hash? v) (hash-has-key? m k) (hash? (hash-ref m k)))
           (hash-set m k (deep-merge (hash-ref m k) v))]
          [(and (string? v) (hash-has-key? m k) (string? (hash-ref m k)))
           (hash-set m k (string-append (hash-ref m k) v))]
          [else (hash-set m k v)])))

(define (merge-tool-calls tch tcs)
  (for/fold ([h tch]) ([tc (in-list tcs)])
    (define idx (hash-ref tc 'index 0))
    (hash-set h idx (deep-merge (hash-ref h idx (hasheq)) tc))))
