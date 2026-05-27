#lang racket
;; ============================================================
;; history.rkt — pure tree abstraction for conversation history
;;
;; Each node = one chat round:
;;   input  — the single message hash sent to API (e.g. (hasheq 'role "user" 'content "hi"))
;;   output — the single message hash returned from API (e.g. (hasheq 'role "assistant" 'content "hello"))
;;   root node: input = #f, output = #f
;;
;; Dependencies: none (pure racket/base)
;; Serialization: #:prefab structs, write/read compatible
;; ============================================================

(provide
 (struct-out history-node)
 (struct-out history-root)
 history-make-root
 history-make-node
 history-next
 history-branch-same
 history-branch-new
 history-leaves
 history-path
 history-children
 history-siblings
 history-root-node
 history-cut
 history-replace
 history-delete!
 history-save
 history-load
 history-write
 history-read)

;; ============================================================
;; Structures
;; ============================================================

(struct history-node (input output parent children) #:prefab #:mutable)
(struct history-root (leaves root-node) #:prefab #:mutable)

;; ============================================================
;; Construction
;; ============================================================

(define (history-make-root)
  (define root (history-node #f #f #f '()))
  (history-root (list root) root))

(define (history-make-node input parent)
  (history-node input #f parent '()))

;; ============================================================
;; Linear append
;; ============================================================

(define (history-next root node input)
  (unless (null? (history-node-children node))
    (error 'history-next "node already has children; use history-branch-new"))
  (define child (history-make-node input node))
  (set-history-node-children! node (list child))
  (set-history-root-leaves! root
    (cons child (remove node (history-root-leaves root))))
  (values root child))

;; ============================================================
;; Branching
;; ============================================================

(define (history-branch-same root node)
  (define parent (history-node-parent node))
  (unless parent
    (error 'history-branch-same "cannot branch at root node"))
  (define new-sibling (history-node (history-node-input node) #f parent '()))
  (set-history-node-children! parent
    (append (history-node-children parent) (list new-sibling)))
  (set-history-root-leaves! root
    (cons new-sibling (history-root-leaves root)))
  new-sibling)

(define (history-branch-new root node input)
  (define child (history-make-node input node))
  (define was-leaf? (null? (history-node-children node)))
  (set-history-node-children! node
    (append (history-node-children node) (list child)))
  (define leaves (if was-leaf?
                     (cons child (remove node (history-root-leaves root)))
                     (cons child (history-root-leaves root))))
  (set-history-root-leaves! root leaves)
  (values root child))

;; ============================================================
;; Query
;; ============================================================

(define (history-leaves root) (history-root-leaves root))

(define (history-path node)
  (let loop ([n node] [acc '()])
    (if n
        (loop (history-node-parent n) (cons n acc))
        acc)))

(define (history-children node) (history-node-children node))

(define (history-siblings node)
  (define parent (history-node-parent node))
  (if parent
      (remove node (history-node-children parent))
      '()))

(define (history-root-node root) (history-root-root-node root))

;; ============================================================
;; Deletion
;; ============================================================

(define (collect-subtree-leaves node)
  (if (null? (history-node-children node))
      (list node)
      (append-map collect-subtree-leaves (history-node-children node))))

(define (history-delete! root node)
  (define parent (history-node-parent node))
  (unless parent
    (error 'history-delete! "cannot delete root node"))
  (define subtree-leaves (collect-subtree-leaves node))
  (set-history-node-children! parent
    (remove node (history-node-children parent)))
  (define new-leaves
    (let ([leaves (history-root-leaves root)])
      (define cleaned
        (filter (lambda (l) (not (memq l subtree-leaves))) leaves))
      (if (null? (history-node-children parent))
          (cons parent cleaned)
          cleaned)))
  (set-history-root-leaves! root new-leaves)
  (set-history-node-parent! node #f)
  root)

;; ============================================================
;; Cut & Replace
;; ============================================================

;; history-cut: cut (remove) a node and its entire subtree from the tree,
;; returning the detached node as a standalone subtree root.
;; - Cannot cut the root node.
;; - Parent's children list is updated.
;; - Leaves are updated: subtree leaves removed; parent added back if it becomes a leaf.
;; - The cut node's parent is set to #f.
(define (history-cut root node)
  (define parent (history-node-parent node))
  (unless parent
    (error 'history-cut "cannot cut root node"))
  ;; Remove node from parent's children
  (set-history-node-children! parent
    (remove node (history-node-children parent)))
  ;; Collect subtree leaves and remove from root leaves
  (define subtree-leaves (collect-subtree-leaves node))
  (set-history-root-leaves! root
    (filter (lambda (l) (not (memq l subtree-leaves)))
            (history-root-leaves root)))
  ;; If parent becomes leaf, add it back
  (when (null? (history-node-children parent))
    (set-history-root-leaves! root
      (cons parent (history-root-leaves root))))
  ;; Detach node
  (set-history-node-parent! node #f)
  (values root node))

;; history-replace: replace old-node with new-node in the tree.
;; new-node inherits old-node's parent and children, preserving the tree structure.
;; - Cannot replace the root node.
;; - Parent's children list is updated (old → new).
;; - Children of old-node get their parent reference updated to new-node.
;; - Leaves are updated: old-node's subtree leaves → new-node's subtree leaves.
;; - old-node is detached (parent = #f, children cleared).
(define (history-replace root old-node new-node)
  (define parent (history-node-parent old-node))
  (unless parent
    (error 'history-replace "cannot replace root node"))
  ;; Collect leaves of old-node's subtree (before modification)
  (define old-subtree-leaves (collect-subtree-leaves old-node))
  ;; Replace old-node with new-node in parent's children
  (set-history-node-children! parent
    (map (lambda (c) (if (eq? c old-node) new-node c))
         (history-node-children parent)))
  ;; new-node inherits old-node's parent and children
  (set-history-node-parent! new-node parent)
  (set-history-node-children! new-node (history-node-children old-node))
  ;; Update children's parent references to new-node
  (for-each (lambda (c) (set-history-node-parent! c new-node))
            (history-node-children new-node))
  ;; Collect leaves of new-node's subtree (after inheritance)
  (define new-subtree-leaves (collect-subtree-leaves new-node))
  ;; Update root leaves: remove old leaves, add new leaves
  (define remaining
    (filter (lambda (l) (not (memq l old-subtree-leaves)))
            (history-root-leaves root)))
  (set-history-root-leaves! root
    (append new-subtree-leaves remaining))
  ;; Detach old node
  (set-history-node-parent! old-node #f)
  (set-history-node-children! old-node '())
  (values root old-node))

;; ============================================================
;; Serialization
;; ============================================================

(define (history-write root)
  (with-output-to-string (lambda () (write root))))

(define (history-read str)
  (with-input-from-string str read))

(define (history-save root path)
  (with-output-to-file path
    (lambda () (write root))
    #:exists 'replace))

(define (history-load path)
  (with-input-from-file path read))

