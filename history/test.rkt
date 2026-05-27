#lang racket

(require "history.rkt" rackunit)

;; ============================================================
;; NOTE: Tests use the full history- prefix (no h- abbreviation)
;; ============================================================

;; helpers
(define (u content) (hasheq 'role "user" 'content content))
(define (a content) (hasheq 'role "assistant" 'content content))

;; ============================================================
;; 1. Construction
;; ============================================================

(test-case "history-make-root"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (check-equal? (length (history-leaves root)) 1)
  (check-false (history-node-input rn))
  (check-false (history-node-output rn))
  (check-false (history-node-parent rn))
  (check-equal? (history-node-children rn) '()))

(test-case "history-make-node"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define n (history-make-node (u "hi") rn))
  (check-equal? (history-node-input n) (u "hi"))
  (check-false (history-node-output n))
  (check-true (eq? (history-node-parent n) rn))
  (check-equal? (history-node-children n) '())
  (check-equal? (length (history-leaves root)) 1))

;; ============================================================
;; 2. history-next
;; ============================================================

(test-case "history-next linear"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "hello")))
  (check-equal? (history-node-input n1) (u "hello"))
  (check-eq? (history-node-parent n1) rn)
  (check-equal? (length (history-leaves r1)) 1)
  (check-true (eq? (car (history-leaves r1)) n1))
  (set-history-node-output! n1 (a "hi"))
  (check-equal? (history-node-output n1) (a "hi"))
  (define-values (r2 n2) (history-next r1 n1 (u "how are you")))
  (check-equal? (history-node-input n2) (u "how are you"))
  (check-equal? (length (history-leaves r2)) 1)
  (check-true (eq? (car (history-leaves r2)) n2)))

(test-case "history-next from non-leaf errors"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "q1")))
  (define-values (r2 n2) (history-next r1 n1 (a "a1")))
  (check-exn #rx"already has children"
    (lambda () (history-next r2 n1 (u "q3")))))

;; ============================================================
;; 3. Branching
;; ============================================================

(test-case "history-branch-same"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "question")))
  (define n2 (history-branch-same r1 n1))
  (check-equal? (history-node-input n2) (u "question"))
  (check-false (history-node-output n2))
  (check-eq? (history-node-parent n2) (history-node-parent n1))
  (check-equal? (length (history-siblings n1)) 1)
  (check-equal? (length (history-leaves r1)) 2))

(test-case "history-branch-same at root errors"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (check-exn #rx"cannot branch at root"
    (lambda () (history-branch-same root rn))))

(test-case "history-branch-new from leaf"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "q1")))
  (define-values (r2 n2) (history-branch-new r1 n1 (u "q1-alt")))
  (check-equal? (history-node-input n2) (u "q1-alt"))
  (check-eq? (history-node-parent n2) n1)
  (check-equal? (length (history-leaves r2)) 1)
  (check-true (eq? (car (history-leaves r2)) n2)))

(test-case "history-branch-new from root (multiple trees)"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-branch-new root rn (u "tree1")))
  (define-values (r2 n2) (history-branch-new r1 rn (u "tree2")))
  (check-equal? (length (history-node-children rn)) 2)
  (check-equal? (length (history-leaves r2)) 2))


;; ============================================================
;; 4. Query
;; ============================================================

(test-case "history-path"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "u1")))
  (define-values (r2 n2) (history-next r1 n1 (a "a1")))
  (define p (history-path n2))
  (check-equal? (length p) 3)
  (check-eq? (car p) rn)
  (check-eq? (cadr p) n1)
  (check-eq? (caddr p) n2))

(test-case "history-root-node / history-children / history-siblings"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "q1")))
  (define n2 (history-branch-same r1 n1))
  (check-equal? (history-children rn) (list n1 n2))
  (check-equal? (history-siblings n1) (list n2))
  (check-true (eq? (history-root-node r1) rn)))

;; ============================================================
;; 5. history-delete!
;; ============================================================

(test-case "history-delete! root errors"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (check-exn #rx"cannot delete root"
    (lambda () (history-delete! root rn))))

(test-case "history-delete! leaf"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "q1")))
  (define-values (r2 n2) (history-next r1 n1 (a "a1")))
  (history-delete! r2 n2)
  (check-equal? (length (history-leaves r2)) 1)
  (check-true (ormap (lambda (l) (eq? l n1)) (history-leaves r2))))

(test-case "history-delete! subtree"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "q1")))
  (define-values (r2 n2) (history-next r1 n1 (a "a1")))
  (define-values (r3 n3) (history-branch-new r2 n1 (a "a1-alt")))
  (history-delete! r3 n1)
  (check-equal? (length (history-leaves r3)) 1)
  (check-true (eq? (car (history-leaves r3)) (history-root-node r3))))

;; ============================================================
;; 6. history-cut
;; ============================================================

(test-case "history-cut root errors"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (check-exn #rx"cannot cut root"
    (lambda () (history-cut root rn))))

(test-case "history-cut leaf"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "q1")))
  (set-history-node-output! n1 (a "a1"))
  (define-values (r2 cut) (history-cut r1 n1))
  ;; After cut: rn should be leaf again
  (check-equal? (length (history-leaves r2)) 1)
  (check-true (eq? (car (history-leaves r2)) rn))
  ;; Cut node should be detached
  (check-false (history-node-parent cut))
  (check-equal? (history-node-input cut) (u "q1"))
  (check-equal? (history-node-output cut) (a "a1"))
  ;; Parent should have no children
  (check-equal? (history-node-children rn) '()))

(test-case "history-cut internal node"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "q1")))
  (define-values (r2 n2) (history-next r1 n1 (a "a1")))
  (define-values (r3 n3) (history-next r2 n2 (u "q2")))
  ;; Cut n1 (internal node) - should remove n1, n2, n3
  (define-values (r4 cut) (history-cut r3 n1))
  ;; Root should be leaf again
  (check-equal? (length (history-leaves r4)) 1)
  (check-true (eq? (car (history-leaves r4)) rn))
  ;; Cut node detached
  (check-false (history-node-parent cut))
  (check-eq? cut n1)
  ;; cut's subtree should be intact (internally consistent)
  (check-equal? (length (history-node-children cut)) 1)
  (check-eq? (car (history-node-children cut)) n2))

(test-case "history-cut from branching tree"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "q1")))
  (define n2 (history-branch-same r1 n1))
  (define-values (r2 n3) (history-next r1 n1 (u "q1-followup")))
  ;; Now rn has 2 children: n1 and n2. Leaves: n2, n3
  (check-equal? (length (history-leaves r2)) 2)
  ;; Cut n1 (and its child n3)
  (define-values (r3 cut) (history-cut r2 n1))
  ;; Leaves should be just n2
  (check-equal? (length (history-leaves r3)) 1)
  (check-true (eq? (car (history-leaves r3)) n2))
  ;; rn should have only n2 as child
  (check-equal? (history-node-children rn) (list n2)))

;; ============================================================
;; 7. history-replace
;; ============================================================

(test-case "history-replace root errors"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define new-node (history-make-node (u "new") #f))
  (check-exn #rx"cannot replace root"
    (lambda () (history-replace root rn new-node))))

(test-case "history-replace leaf with new leaf"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "original")))
  (set-history-node-output! n1 (a "original-reply"))
  (define replacement (history-make-node (u "replacement") #f))
  (define-values (r2 old) (history-replace r1 n1 replacement))
  ;; old should be detached
  (check-false (history-node-parent old))
  (check-equal? (history-node-input old) (u "original"))
  ;; replacement should be in the tree
  (check-true (eq? (history-node-parent replacement) rn))
  (check-equal? (history-node-children rn) (list replacement))
  ;; leaves should now contain replacement, not old
  (check-equal? (length (history-leaves r2)) 1)
  (check-true (eq? (car (history-leaves r2)) replacement)))

(test-case "history-replace internal node preserves children chain"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "q1")))
  (define-values (r2 n2) (history-next r1 n1 (a "a1")))
  (define-values (r3 n3) (history-next r2 n2 (u "q2")))
  ;; Replace n1 with a new node; n2, n3 chain should be preserved
  (define replacement (history-make-node (u "new-q1") #f))
  (define-values (r4 old) (history-replace r3 n1 replacement))
  ;; old detached
  (check-false (history-node-parent old))
  ;; replacement has n2 as child
  (check-equal? (length (history-node-children replacement)) 1)
  (check-true (eq? (car (history-node-children replacement)) n2))
  ;; n2's parent is now replacement
  (check-true (eq? (history-node-parent n2) replacement))
  ;; leaves unchanged (n3 still leaf)
  (check-equal? (length (history-leaves r4)) 1)
  (check-true (eq? (car (history-leaves r4)) n3))
  ;; path from leaf should go through replacement
  (define p (history-path n3))
  (check-equal? (length p) 4)
  (check-eq? (list-ref p 0) rn)
  (check-eq? (list-ref p 1) replacement)
  (check-eq? (list-ref p 2) n2)
  (check-eq? (list-ref p 3) n3))

(test-case "history-replace in branching tree"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "q1")))
  (define n2 (history-branch-same r1 n1))
  (define-values (r2 n3) (history-next r1 n1 (u "followup")))
  ;; rn children: n1, n2. Leaves: n2, n3
  (define replacement (history-make-node (u "replacement") #f))
  (define-values (r3 old) (history-replace r2 n1 replacement))
  ;; rn children: replacement, n2
  (check-equal? (length (history-node-children rn)) 2)
  (check-not-false (memq replacement (history-node-children rn)))
  (check-not-false (memq n2 (history-node-children rn)))
  ;; replacement inherits n3 as child
  (check-equal? (history-node-children replacement) (list n3))
  (check-true (eq? (history-node-parent n3) replacement))
  ;; leaves: n2, n3
  (check-equal? (length (history-leaves r3)) 2)
  (check-not-false (memq n2 (history-leaves r3)))
  (check-not-false (memq n3 (history-leaves r3))))

;; ============================================================
;; 8. Serialization
;; ============================================================

(test-case "history-write/history-read roundtrip"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "q1")))
  (set-history-node-output! n1 (a "a1"))
  (define s (history-write r1))
  (define d (history-read s))
  (check-pred history-root? d)
  (check-equal? (length (history-leaves d)) 1))

(test-case "history-save/history-load roundtrip"
  (define root (history-make-root))
  (define rn (history-root-node root))
  (define-values (r1 n1) (history-next root rn (u "save-q")))
  (set-history-node-output! n1 (a "save-a"))
  (define tmp (make-temporary-file "hist-~a.rktd"))
  (history-save r1 tmp)
  (define loaded (history-load tmp))
  (check-pred history-root? loaded)
  (check-equal? (length (history-leaves loaded)) 1)
  (define ln (car (history-leaves loaded)))
  (check-equal? (history-node-input ln) (u "save-q"))
  (check-equal? (history-node-output ln) (a "save-a"))
  (delete-file tmp))

