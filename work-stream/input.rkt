#lang racket
;; ============================================================
;; work-stream/input.rkt — 非阻塞 \n 中断检测
;;
;; 用法:
;;   (define stop? (make-newline-stop?))
;;   ;; 传给 env-chat/stream 的 #:stop? 参数
;;   ;; 用户在流式输出过程中按 Enter 即中断
;; ============================================================

(provide make-newline-stop?)

(define (make-newline-stop?)
  (define stopped? #f)
  (define buf #f)
  (lambda ()
    (cond
      [stopped? #t]
      [buf
       (define c buf)
       (set! buf #f)
       (when (char=? c #\newline)
         (set! stopped? #t))
       stopped?]
      [(char-ready? (current-input-port))
       (define c (read-char))
       (cond
         [(eof-object? c) stopped?]
         [(char=? c #\newline)
          (set! stopped? #t)
          #t]
         [else
          (set! buf c)
          #f])]
      [else #f])))
