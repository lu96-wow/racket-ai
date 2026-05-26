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

 ;; —— tree workflow primitives ——
 h-continue              ; h-root? h-node? string? -> (values h-root? h-node?)
                         ;   叶子→h-next, 有子→h-branch-new
 h-retry-from            ; h-root? h-node? -> h-node?
                         ;   从任意节点用相同输入创建兄弟分支
 h-range-nodes           ; h-node? h-node? -> (listof h-node?)
                         ;   从 start 到 end 沿路径的节点列表
 h-squash-range          ; h-root? h-node? h-node? -> (values h-root? h-node? (listof pair?))
                         ;   折叠 start→end 子链，返回占位节点和对话 pairs
 h-re-root               ; h-root? h-node? -> h-root?
                         ;   以 node 为根创建新树

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
;; Tree Workflow Primitives
;; ============================================================

;; h-continue : h-root? h-node? string? -> (values h-root? h-node?)
;; Continue conversation from `node` with `user-content`.
;; If `node` is a leaf (no children), behaves like h-next (linear append).
;; If `node` already has children, auto-branches via h-branch-new.
;; This is the "auto-branch" primitive for non-linear workflows.
(define (h-continue root node user-content)
  (if (null? (h-node-children node))
      (h-next root node user-content)
      (h-branch-new root node user-content)))

;; h-retry-from : h-root? h-node? -> h-node?
;; Create a sibling branch for `node` with the SAME user-content.
;; This is a generalization of h-branch-same: you can retry from any
;; node in the history, not just the current one.
;; Returns the new sibling node (which is a leaf, ready for AI response).
(define (h-retry-from root node)
  (h-branch-same root node))

;; h-range-nodes : h-node? h-node? -> (listof h-node?)
;; Collect all nodes on the direct path from `start-node` to `end-node`
;; (both inclusive).  Raises an error if end-node is not a descendant
;; of start-node along the linear chain.
(define (h-range-nodes start-node end-node)
  (define end-path (h-path end-node))
  (define start-pos
    (let loop ([path end-path] [idx 0])
      (cond
        [(null? path) #f]
        [(eq? (car path) start-node) idx]
        [else (loop (cdr path) (add1 idx))])))
  (unless start-pos
    (error 'h-range-nodes
           "end-node is not a descendant of start-node along the direct path"))
  (list-tail end-path start-pos))

;; h-squash-range : h-root? h-node? h-node? -> (values h-root? h-node? (listof pair?))
;; Squash the conversation range from `start-node` to `end-node` (inclusive)
;; into a single placeholder node.
;;
;; What happens:
;;   1. All nodes from start-node to end-node (the range) are removed.
;;   2. A new node (user-content = start-node's user-content) replaces
;;      start-node in the tree.
;;   3. end-node's children are re-parented to the new node.
;;   4. Returns the updated root, the new placeholder node, and the
;;      list of (user . ai) pairs for the squashed range.
;;
;; The caller should use h-set-ai! on the returned new-node with an
;; AI-generated summary of the returned pairs.
(define (h-squash-range root start-node end-node)
  (define range-nodes (h-range-nodes start-node end-node))
  (define parent (h-node-parent start-node))
  (unless parent
    (error 'h-squash-range "cannot squash from root node"))

  ;; 1. Collect user/ai pairs for the range
  (define pairs
    (for/list ([n (in-list range-nodes)])
      (cons (h-node-user-content n) (h-node-ai-content n))))

  ;; 2. Collect all leaves in start-node's subtree (before any mutation)
  (define old-subtree-leaves (collect-subtree-leaves start-node))

  ;; 3. Collect leaves of end-node's subtree (may include end-node itself)
  (define end-subtree-leaves
    (if (null? (h-node-children end-node))
        (list end-node)
        (collect-subtree-leaves end-node)))

  ;; 4. Create new replacement node at start-node's position
  (define new-node (make-node (h-node-user-content start-node) parent))

  ;; 5. Transfer end-node's children to new-node
  (define end-children (h-node-children end-node))
  (for ([child (in-list end-children)])
    (set-h-node-parent! child new-node))
  (set-h-node-children! new-node end-children)
  (set-h-node-children! end-node '())   ;; cleanup

  ;; 6. Replace start-node with new-node in parent's children list
  (set-h-node-children! parent
    (map (lambda (c) (if (eq? c start-node) new-node c))
         (h-node-children parent)))

  ;; 7. Update root's leaves
  ;;    Remove all leaves from start's subtree, then add back
  ;;    the leaves that are now under new-node.
  (define new-leaves-for-subtree
    (if (null? end-children)
        (list new-node)            ;; new-node becomes the only leaf
        end-subtree-leaves))       ;; end's children's leaves are preserved
  (set-h-root-leaves! root
    (append (filter (lambda (l) (not (memq l old-subtree-leaves)))
                    (h-root-leaves root))
            new-leaves-for-subtree))

  ;; 8. Clear parent pointers for removed nodes (safety / GC)
  (for ([n (in-list range-nodes)] #:unless (eq? n end-node))
    (set-h-node-parent! n #f))

  (values root new-node pairs))

;; h-re-root : h-root? h-node? -> h-root?
;; Re-root the conversation tree at `node`.  Creates a fresh h-root
;; with `node` as the sole child of a new sentinel root.  All
;; descendants of `node` are preserved; ancestors are discarded.
;;
;; This is useful for pruning a long history to focus on a subtree,
;; or for making a branch the new main conversation.
(define (h-re-root root node)
  (define new-sentinel (h-node #f #f #f '()))

  ;; Detach node from its old parent (if any)
  (define old-parent (h-node-parent node))
  (when old-parent
    (set-h-node-children! old-parent
      (remove node (h-node-children old-parent))))

  ;; Attach node to new sentinel
  (set-h-node-children! new-sentinel (list node))
  (set-h-node-parent! node new-sentinel)

  ;; Collect all leaves in node's subtree
  (define subtree-leaves (collect-subtree-leaves node))

  ;; Return new h-root
  (h-root subtree-leaves new-sentinel))

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
    (check-equal? (h-node-ai-content (list-ref p (sub1 (length p)))) "b"))

  ;; --- h-continue from leaf (linear) ---
  (test-case "h-continue from leaf behaves like h-next"
    (define root (make-root))
    (define-values (r1 n1) (h-continue root (h-root-node root) "q1"))
    (h-set-ai! r1 n1 "a1")
    (check-equal? (length (h-leaves r1)) 1)
    (check-true (eq? (car (h-leaves r1)) n1))
    (check-equal? (h-node-user-content n1) "q1"))

  ;; --- h-continue from non-leaf (auto-branch) ---
  (test-case "h-continue from non-leaf auto-branches"
    (define root (make-root))
    (define-values (r1 n1) (h-next root (h-root-node root) "q1"))
    (h-set-ai! r1 n1 "a1")
    (define-values (r2 n2) (h-next r1 n1 "q2"))
    (h-set-ai! r2 n2 "a2")
    ;; now n1 has a child; h-continue from n1 should branch
    (define-values (r3 n3) (h-continue r2 n1 "q2-branch"))
    (check-equal? (h-node-user-content n3) "q2-branch")
    (check-equal? (length (h-node-children n1)) 2)
    (check-equal? (length (h-leaves r3)) 2))

  ;; --- h-retry-from ---
  (test-case "h-retry-from creates sibling"
    (define root (make-root))
    (define-values (r1 n1) (h-next root (h-root-node root) "question"))
    (h-set-ai! r1 n1 "answer 1")
    (define n2 (h-retry-from r1 n1))
    (check-equal? (h-node-user-content n2) "question")
    (check-false (h-node-ai-content n2))
    (check-equal? (length (h-leaves r1)) 2)
    (check-true (ormap (lambda (l) (eq? l n2)) (h-leaves r1))))

  ;; --- h-range-nodes ---
  (test-case "h-range-nodes linear path"
    (define root (make-root))
    (define rn (h-root-node root))
    (define-values (r1 n1) (h-next root rn "u1"))
    (define-values (r2 n2) (h-next r1 n1 "u2"))
    (define-values (r3 n3) (h-next r2 n2 "u3"))
    (define range (h-range-nodes n1 n3))
    (check-equal? (length range) 3)
    (check-eq? (car range) n1)
    (check-eq? (cadr range) n2)
    (check-eq? (caddr range) n3))

  (test-case "h-range-nodes error when not ancestor"
    (define root (make-root))
    (define rn (h-root-node root))
    (define-values (r1 n1) (h-next root rn "u1"))
    (define-values (r2 n2) (h-next r1 n1 "u2"))
    (define-values (r3 n3) (h-next r2 n2 "u3"))
    (check-exn #rx"not a descendant"
               (lambda () (h-range-nodes n3 n1))))

  ;; --- h-squash-range ---
  (test-case "h-squash-range replaces chain with one node (end is leaf)"
    (define root (make-root))
    (define rn (h-root-node root))
    (define-values (r1 n1) (h-next root rn "q1"))
    (h-set-ai! r1 n1 "a1")
    (define-values (r2 n2) (h-next r1 n1 "q2"))
    (h-set-ai! r2 n2 "a2")
    (define-values (r3 n3) (h-next r2 n2 "q3"))
    (h-set-ai! r3 n3 "a3")
    ;; squash n1→n3
    (define-values (r4 new-node pairs) (h-squash-range r3 n1 n3))
    ;; check: only 1 leaf (new-node)
    (check-equal? (length (h-leaves r4)) 1)
    (check-true (eq? (car (h-leaves r4)) new-node))
    ;; check pairs
    (check-equal? (length pairs) 3)
    (check-equal? (car (car pairs)) "q1")
    (check-equal? (cdr (caddr pairs)) "a3")
    ;; check path
    (define path (h-path new-node))
    (check-equal? (length path) 2)  ;; sentinel + new-node
    (check-equal? (h-node-user-content new-node) "q1")
    ;; no ai content yet (to be filled by caller)
    (check-false (h-node-ai-content new-node)))

  (test-case "h-squash-range preserves end-node's children"
    (define root (make-root))
    (define rn (h-root-node root))
    (define-values (r1 n1) (h-next root rn "q1"))
    (h-set-ai! r1 n1 "a1")
    (define-values (r2 n2) (h-next r1 n1 "q2"))
    (h-set-ai! r2 n2 "a2")
    (define-values (r3 n3) (h-next r2 n2 "q3"))
    (h-set-ai! r3 n3 "a3")
    (define-values (r4 n4) (h-next r3 n3 "q4"))
    (h-set-ai! r4 n4 "a4")
    ;; squash n1→n3 (n3 has child n4)
    (define-values (r5 new-node pairs) (h-squash-range r4 n1 n3))
    ;; n4 should still be the leaf
    (check-equal? (length (h-leaves r5)) 1)
    (check-true (eq? (car (h-leaves r5)) n4))
    ;; new-node should have n4 as child
    (check-equal? (length (h-node-children new-node)) 1)
    (check-true (eq? (car (h-node-children new-node)) n4))
    (check-true (eq? (h-node-parent n4) new-node))
    ;; pairs should have 3 entries
    (check-equal? (length pairs) 3))

  ;; --- h-re-root ---
  (test-case "h-re-root makes node the new root"
    (define root (make-root))
    (define rn (h-root-node root))
    (define-values (r1 n1) (h-next root rn "q1"))
    (h-set-ai! r1 n1 "a1")
    (define-values (r2 n2) (h-next r1 n1 "q2"))
    (h-set-ai! r2 n2 "a2")
    (define-values (r3 n3) (h-next r2 n2 "q3"))
    (h-set-ai! r3 n3 "a3")
    ;; re-root at n1
    (define new-root (h-re-root r3 n1))
    (check-true (h-root? new-root))
    (check-equal? (length (h-leaves new-root)) 1)
    (check-true (eq? (car (h-leaves new-root)) n3))
    ;; path from n3 in new root
    (define path (h-path n3))
    (check-equal? (length path) 4)  ;; new-sentinel, n1, n2, n3
    (check-false (h-node-user-content (car path)))  ;; sentinel has #f content
    (check-eq? (cadr path) n1))
)
