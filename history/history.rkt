#lang racket
;; ============================================================
;; history.rkt — composable, minimal-dependency conversation history
;;
;; Design:
;;   单向链表 (singly-linked tree via parent pointer)
;;   h-node  — 单个对话轮次（用户输入 + AI 输出）
;;   h-root  — 根容器，持有全部叶子节点引用
;;
;;   leaf invariant: h-root 的 leaves 列表精确包含所有 children
;;   为空的 h-node。当叶子获得子节点时，从 leaves 移除，
;;   并将新子节点加入 leaves。
;;
;; Branching:
;;   h-branch-same  — 相同用户输入，期待不同 AI 输出
;;   h-branch-new   — 不同用户输入，延续新分支
;;
;; Dependencies: 无（纯 racket/base）
;; Serialization: #:prefab 结构体，可直接 write/read
;; ============================================================

(provide
 ;; —— structures ——
 (struct-out h-node)
 (struct-out h-root)

 ;; —— construction ——
 make-root               ; -> h-root?
 make-node               ; string? h-node? -> h-node?

 ;; —— conversation ——
 h-next                  ; h-root? h-node? string? -> (values h-root? h-node?)
 h-set-ai!               ; h-root? h-node? (or/c string? hasheq?) -> h-root?

 ;; —— branching ——
 h-branch-same           ; h-root? h-node? -> h-node?
 h-branch-new            ; h-root? h-node? string? -> (values h-root? h-node?)

 ;; —— query ——
 h-leaves                ; h-root? -> (listof h-node?)
 h-path                  ; h-node? -> (listof h-node?)   root→node
 h-children              ; h-node? -> (listof h-node?)
 h-siblings              ; h-node? -> (listof h-node?)
 h-root-node             ; h-root? -> h-node?
 h-node-content          ; h-node? -> (values string? (or/c string? hasheq? #f))

 ;; —— mutation ——
 h-delete!               ; h-root? h-node? -> h-root?

 ;; —— bulk construction ——
 build-linear-history    ; (listof (cons/c string? (or/c string? hasheq?)))
                         ;   -> (values h-root? h-node?)

 ;; —— serialization ——
 h-save                  ; h-root? path-string? -> void?
 h-load                  ; path-string? -> h-root?
 h-write                 ; h-root? -> string?
 h-read                  ; string? -> h-root?
 )



;; ============================================================
;; Structures — #:prefab for zero-dependency serialization
;; ============================================================

;; h-node: a single conversation turn.
;;   user-content : string?          — what the user said
;;   ai-content   : (or/c string? hasheq? #f) — AI response
;;   parent       : (or/c h-node? #f) — back-pointer (singly-linked)
;;   children     : (listof h-node?) — forward branches
(struct h-node (user-content ai-content parent children) #:prefab #:mutable)

;; h-root: conversation history root container.
;;   leaves    : (listof h-node?) — all current leaf nodes
;;   root-node : h-node?         — sentinel root (user-content = #f)
;;
;; Invariant: every h-node with children='() appears exactly once in leaves.
(struct h-root (leaves root-node) #:prefab #:mutable)

;; ============================================================
;; Construction
;; ============================================================

;; make-root : -> h-root?
;; Create an empty history root.
(define (make-root)
  (define root (h-node #f #f #f '()))
  (h-root (list root) root))

;; make-node : string? h-node? -> h-node?
;; Create a bare node (not attached to tree).  Use h-next / h-branch-*
;; to attach it to the history.
(define (make-node user-content parent)
  (h-node user-content #f parent '()))

;; ============================================================
;; Conversation — linear walk
;; ============================================================

;; h-next : h-root? h-node? string? -> (values h-root? h-node?)
;; Add a user message as the next turn, continuing linearly from `node`.
;; `node` MUST be a leaf (otherwise it's a branch — use h-branch-new).
;; Removes `node` from root leaves, adds new child to leaves.
(define (h-next root node user-content)
  (unless (null? (h-node-children node))
    (error 'h-next "node already has children; use h-branch-new"))
  (define child (make-node user-content node))
  (set-h-node-children! node (list child))
  ;; leaf management: remove node, add child
  (set-h-root-leaves! root
    (cons child (remove node (h-root-leaves root))))
  (values root child))

;; h-set-ai! : h-root? h-node? (or/c string? hasheq?) -> h-root?
;; Set the AI response for a node.  Returns root (mutated in place).
(define (h-set-ai! root node ai-content)
  (set-h-node-ai-content! node ai-content)
  root)

;; ============================================================
;; Branching
;; ============================================================

;; h-branch-same : h-root? h-node? -> h-node?
;; Branch at `node`: re-use the SAME user-content, expecting a
;; different AI response.  The new node becomes a sibling of `node`.
;; `node` must have a parent (cannot branch at root).
;;
;; This is the "regenerate" pattern: same question, different answer.
(define (h-branch-same root node)
  (define parent (h-node-parent node))
  (unless parent
    (error 'h-branch-same "cannot branch at root node"))
  (define new-sibling (make-node (h-node-user-content node) parent))
  ;; add to parent's children
  (set-h-node-children! parent
    (append (h-node-children parent) (list new-sibling)))
  ;; new sibling has no children → it IS a leaf
  (set-h-root-leaves! root
    (cons new-sibling (h-root-leaves root)))
  new-sibling)

;; h-branch-new : h-root? h-node? string? -> (values h-root? h-node?)
;; Branch at `node` with NEW user-content.  The new node becomes
;; a child of `node`.  If `node` was a leaf, it is replaced in
;; leaves by the new child; otherwise the new child is simply added.
;;
;; When `node` is the root (which has no user-content), this
;; effectively starts a new conversation tree from scratch.
;; Multiple calls from root create parallel, independent trees.
(define (h-branch-new root node user-content)
  (define child (make-node user-content node))
  ;; leaf management — check BEFORE mutation
  (define was-leaf? (null? (h-node-children node)))
  ;; add to node's children
  (set-h-node-children! node
    (append (h-node-children node) (list child)))
  (define leaves (if was-leaf?
                     (cons child (remove node (h-root-leaves root)))
                     (cons child (h-root-leaves root))))
  (set-h-root-leaves! root leaves)
  (values root child))


;; ============================================================
;; Query
;; ============================================================

;; h-leaves : h-root? -> (listof h-node?)
(define (h-leaves root)
  (h-root-leaves root))

;; h-path : h-node? -> (listof h-node?)
;; Return the path from root down to `node` (inclusive).
(define (h-path node)
  (let loop ([n node] [acc '()])
    (if n
        (loop (h-node-parent n) (cons n acc))
        acc)))

;; h-children : h-node? -> (listof h-node?)
(define (h-children node)
  (h-node-children node))

;; h-siblings : h-node? -> (listof h-node?)
;; Return siblings of `node` (other children of same parent).
(define (h-siblings node)
  (define parent (h-node-parent node))
  (if parent
      (remove node (h-node-children parent))
      '()))

;; h-root-node : h-root? -> h-node?
(define (h-root-node root)
  (h-root-root-node root))

;; h-node-content : h-node? -> (values string? (or/c string? hasheq? #f))
;; Extract user-content and ai-content from a node.
(define (h-node-content node)
  (values (h-node-user-content node)
          (h-node-ai-content node)))

;; ============================================================
;; Bulk construction
;; ============================================================

;; build-linear-history : (listof (cons/c string? (or/c string? hasheq?)))
;;                         -> (values h-root? h-node?)
;;
;; Build a linear conversation history from (user . ai) pairs.
;; Returns the root and the last node (deepest leaf).
;;
;; Example:
;;   (build-linear-history
;;    (list (cons "你好" "你好！有什么可以帮你？")
;;          (cons "天气怎么样" "今天晴天，25°C")))
(define (build-linear-history pairs)
  (define root (make-root))
  (define rn (h-root-node root))
  (let loop ([parent rn]
             [remaining pairs]
             [rt root])
    (if (null? remaining)
        (values rt parent)
        (let ([pair (car remaining)])
          (define-values (new-root child)
            (h-next rt parent (car pair)))
          (h-set-ai! new-root child (cdr pair))
          (loop child (cdr remaining) new-root)))))


;; ============================================================
;; Mutation — delete
;; ============================================================

;; collect-subtree-leaves : h-node? -> (listof h-node?)
;; Collect all leaf nodes in the subtree rooted at `node`.
(define (collect-subtree-leaves node)
  (if (null? (h-node-children node))
      (list node)
      (append-map collect-subtree-leaves (h-node-children node))))

;; h-delete! : h-root? h-node? -> h-root?
;; Delete `node` and its entire subtree from the history.
;; Updates parent's children and root's leaves accordingly.
;; The root node itself cannot be deleted.
(define (h-delete! root node)
  (define parent (h-node-parent node))
  (unless parent
    (error 'h-delete! "cannot delete root node"))
  ;; 1. Collect all leaf nodes in the subtree to be removed
  (define subtree-leaves (collect-subtree-leaves node))
  ;; 2. Remove node from parent's children list
  (set-h-node-children! parent
    (remove node (h-node-children parent)))
  ;; 3. Update root's leaves: remove subtree leaves;
  ;;    if parent became a leaf, add it back
  (define new-leaves
    (let ([leaves (h-root-leaves root)])
      (define cleaned
        (filter (lambda (l) (not (memq l subtree-leaves))) leaves))
      (if (null? (h-node-children parent))
          (cons parent cleaned)
          cleaned)))
  (set-h-root-leaves! root new-leaves)
  ;; 4. Clear deleted node's parent (safety / GC)
  (set-h-node-parent! node #f)
  root)

;; ============================================================
;; Serialization
;; ============================================================

;; h-write : h-root? -> string?
;; Serialize history to a string (prefab write/read format).
(define (h-write root)
  (with-output-to-string (lambda () (write root))))

;; h-read : string? -> h-root?
;; Deserialize history from a string.
(define (h-read str)
  (with-input-from-string str read))

;; h-save : h-root? path-string? -> void?
;; Save history to a file.
(define (h-save root path)
  (with-output-to-file path
    (lambda () (write root))
    #:exists 'replace))

;; h-load : path-string? -> h-root?
;; Load history from a file.
(define (h-load path)
  (with-input-from-file path read))

;; ============================================================
;; Tests
;; ============================================================
(module+ test
  (require rackunit)

  ;; --- construction ---
  (test-case "make-root creates empty history"
    (define root (make-root))
    (check-equal? (length (h-leaves root)) 1)
    (check-false (h-node-user-content (h-root-node root)))
    (check-false (h-node-ai-content (h-root-node root)))
    (check-false (h-node-parent (h-root-node root))))

  ;; --- linear conversation ---
  (test-case "linear conversation with h-next"
    (define root (make-root))
    (define-values (r1 n1) (h-next root (h-root-node root) "hello"))
    (check-equal? (h-node-user-content n1) "hello")
    (check-equal? (length (h-leaves r1)) 1)
    (check-true (eq? (car (h-leaves r1)) n1))

    (h-set-ai! r1 n1 "hi there")
    (check-equal? (h-node-ai-content n1) "hi there")

    (define-values (r2 n2) (h-next r1 n1 "how are you"))
    (check-equal? (h-node-user-content n2) "how are you")
    (check-equal? (length (h-leaves r2)) 1)
    (check-true (eq? (car (h-leaves r2)) n2))

    (h-set-ai! r2 n2 "I'm great!")
    (check-equal? (h-node-ai-content n2) "I'm great!"))

  ;; --- branching: same content ---
  (test-case "h-branch-same creates sibling"
    (define root (make-root))
    (define-values (r1 n1) (h-next root (h-root-node root) "question"))
    (h-set-ai! r1 n1 "answer 1")
    (define n2 (h-branch-same r1 n1))
    (check-equal? (h-node-user-content n2) "question")
    (check-false (h-node-ai-content n2))
    (check-eq? (h-node-parent n2) (h-node-parent n1))
    (check-equal? (length (h-siblings n1)) 1)
    (check-true (eq? (car (h-siblings n1)) n2))
    (check-equal? (length (h-leaves r1)) 2)
    (check-true (ormap (lambda (l) (eq? l n1)) (h-leaves r1)))
    (check-true (ormap (lambda (l) (eq? l n2)) (h-leaves r1))))

  ;; --- branching: new content from non-leaf ---
  (test-case "h-branch-new from non-leaf"
    (define root (make-root))
    (define-values (r1 n1) (h-next root (h-root-node root) "q1"))
    (h-set-ai! r1 n1 "a1")
    (define-values (r2 n2) (h-next r1 n1 "q2"))
    (h-set-ai! r2 n2 "a2")
    (define-values (r3 n3) (h-branch-new r2 n1 "q2-alt"))
    (check-equal? (h-node-user-content n3) "q2-alt")
    (check-eq? (h-node-parent n3) n1)
    (check-equal? (length (h-leaves r3)) 2)
    (check-true (ormap (lambda (l) (eq? l n2)) (h-leaves r3)))
    (check-true (ormap (lambda (l) (eq? l n3)) (h-leaves r3)))
    (check-false (ormap (lambda (l) (eq? l n1)) (h-leaves r3))))

  ;; --- h-path ---
  (test-case "h-path returns root->node chain"
    (define root (make-root))
    (define rn (h-root-node root))
    (define-values (r1 n1) (h-next root rn "u1"))
    (define-values (r2 n2) (h-next r1 n1 "u2"))
    (define path (h-path n2))
    (check-equal? (length path) 3)
    (check-eq? (list-ref path 0) rn)
    (check-eq? (list-ref path 1) n1)
    (check-eq? (list-ref path 2) n2))

  ;; --- h-node-content ---
  (test-case "h-node-content extracts both contents"
    (define root (make-root))
    (define-values (r1 n1) (h-next root (h-root-node root) "ping"))
    (h-set-ai! r1 n1 "pong")
    (define-values (u a) (h-node-content n1))
    (check-equal? u "ping")
    (check-equal? a "pong"))

  ;; --- build-linear-history ---
  (test-case "build-linear-history from pairs"
    (define-values (root last)
      (build-linear-history
       (list (cons "x" "y") (cons "a" "b"))))
    (define path (h-path last))
    (check-equal? (length path) 3)
    (check-equal? (h-node-user-content (list-ref path 1)) "x")
    (check-equal? (h-node-ai-content (list-ref path 1)) "y")
    (check-equal? (h-node-user-content (list-ref path 2)) "a")
    (check-equal? (h-node-ai-content (list-ref path 2)) "b")
    (check-equal? (length (h-leaves root)) 1)
    (check-true (eq? (car (h-leaves root)) last)))

  ;; --- build-linear-history empty ---
  (test-case "build-linear-history empty"
    (define-values (root last)
      (build-linear-history '()))
    (check-eq? last (h-root-node root))
    (check-equal? (length (h-leaves root)) 1))

  ;; --- serialization roundtrip ---
  (test-case "prefab serialization roundtrip"
    (define-values (root last)
      (build-linear-history
       (list (cons "q1" "a1") (cons "q2" "a2"))))
    (define s (with-output-to-string (lambda () (write root))))
    (define d (with-input-from-string s read))
    (check-pred h-root? d)
    (check-equal? (length (h-leaves d)) 1))

  ;; --- delete: leaf node ---
  (test-case "h-delete! removes a leaf"
    (define root (make-root))
    (define-values (r1 n1) (h-next root (h-root-node root) "q1"))
    (h-set-ai! r1 n1 "a1")
    (define-values (r2 n2) (h-next r1 n1 "q2"))
    (h-set-ai! r2 n2 "a2")
    ;; delete n2 (leaf), parent n1 should become leaf
    (check-equal? (length (h-leaves r2)) 1)
    (h-delete! r2 n2)
    (check-equal? (length (h-leaves r2)) 1)
    (check-true (ormap (lambda (l) (eq? l n1)) (h-leaves r2)))
    (check-false (ormap (lambda (l) (eq? l n2)) (h-leaves r2))))

  ;; --- delete: subtree ---
  (test-case "h-delete! removes entire subtree"
    (define root (make-root))
    (define-values (r1 n1) (h-next root (h-root-node root) "q1"))
    (h-set-ai! r1 n1 "a1")
    (define-values (r2 n2) (h-next r1 n1 "q2"))
    (h-set-ai! r2 n2 "a2")
    (define-values (r3 n3) (h-branch-new r2 n1 "q2-alt"))
    (h-set-ai! r3 n3 "a2-alt")
    ;; leaves: n2, n3
    (check-equal? (length (h-leaves r3)) 2)
    ;; delete n1 (which has children n2, n3) — entire subtree goes
    (h-delete! r3 n1)
    ;; only root remains as leaf
    (check-equal? (length (h-leaves r3)) 1)
    (check-true (eq? (car (h-leaves r3)) (h-root-node r3))))

  ;; --- delete: sibling branch ---
  (test-case "h-delete! one sibling leaves the other"
    (define root (make-root))
    (define-values (r1 n1) (h-next root (h-root-node root) "question"))
    (h-set-ai! r1 n1 "answer 1")
    (define n2 (h-branch-same r1 n1))
    (h-set-ai! r1 n2 "answer 2")
    ;; leaves: n1, n2 (two siblings)
    (check-equal? (length (h-leaves r1)) 2)
    ;; delete n1, n2 remains
    (h-delete! r1 n1)
    (check-equal? (length (h-leaves r1)) 1)
    (check-true (eq? (car (h-leaves r1)) n2)))

  ;; --- save/load roundtrip ---
  (test-case "h-save and h-load roundtrip"
    (define-values (root last)
      (build-linear-history
       (list (cons "q1" "a1") (cons "q2" "a2"))))
    (define tmpfile (make-temporary-file "history-test-~a.rktd"))
    (h-save root tmpfile)
    (define loaded (h-load tmpfile))
    (check-pred h-root? loaded)
    (check-equal? (length (h-leaves loaded)) 1)
    (delete-file tmpfile))

  ;; --- h-write / h-read roundtrip ---
  (test-case "h-write and h-read roundtrip"
    (define-values (root last-node)
      (build-linear-history
       (list (cons "a" "b"))))
    (define s (h-write root))
    (define d (h-read s))
    (check-pred h-root? d)
    (check-equal? (length (h-leaves d)) 1)
    (define p (h-path (car (h-leaves d))))
    (check-equal? (h-node-user-content (list-ref p (sub1 (length p)))) "a")
    (check-equal? (h-node-ai-content (list-ref p (sub1 (length p)))) "b")))
