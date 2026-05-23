#lang racket
;; ============================================================
;; format-color/core.rkt — ANSI 颜色/样式系统
;;
;; 纯字节串操作，不涉及光标管理或终端输出。
;; ============================================================

(provide
 ;; 内容转换
 format-content
 ;; ANSI 属性
 format-bold format-dim format-italic format-underline format-blink format-reverse
 format-reset
 ;; 16 色前景/背景
 format-fg format-bg
 ;; 256 色
 format-256-fg format-256-bg
 ;; RGB 色
 format-rgb-fg format-rgb-bg format-rgb-fg-bg
 ;; 样式系统
 style-define! style-reset style->bytes
 format-styled
 ;; 格式化属性（纯字节串）
 format-styled-bold format-styled-dim format-styled-italic
 format-styled-underline format-styled-blink format-styled-reverse
 ;; 颜色构造器（返回字节串）
 color-fg color-bg color256-fg color256-bg color-rgb-fg color-rgb-bg
 ;; 属性常量（字节串）
 attr-bold attr-dim attr-italic attr-underline attr-blink attr-reverse)

;; ============================================================
;; 内容转换
;; ============================================================

(define (format-content v)
  (cond [(bytes? v) v]
        [(string? v) (string->bytes/utf-8 v)]
        [(char? v) (string->bytes/utf-8 (string v))]
        [else (string->bytes/utf-8 (format "~a" v))]))

;; ============================================================
;; ANSI 常量
;; ============================================================

(define format-reset #"\e[0m")
(define format-bold #"\e[1m")
(define format-dim #"\e[2m")
(define format-italic #"\e[3m")
(define format-underline #"\e[4m")
(define format-blink #"\e[5m")
(define format-reverse #"\e[7m")

;; ============================================================
;; 16 色
;; ============================================================

(define (format-fg n)
  (unless (<= 0 n 15) (error 'format-fg "ANSI color must be 0-15, got ~a" n))
  (string->bytes/utf-8 (format "\e[~am" (vector-ref #(30 31 32 33 34 35 36 37
                                                          90 91 92 93 94 95 96 97) n))))

(define (format-bg n)
  (unless (<= 0 n 15) (error 'format-bg "ANSI color must be 0-15, got ~a" n))
  (string->bytes/utf-8 (format "\e[~am" (vector-ref #(40 41 42 43 44 45 46 47
                                                          100 101 102 103 104 105 106 107) n))))

;; ============================================================
;; 256 色
;; ============================================================

(define (format-256-fg n)
  (unless (<= 0 n 255) (error 'format-256-fg "256 color must be 0-255, got ~a" n))
  (string->bytes/utf-8 (format "\e[38;5;~am" n)))

(define (format-256-bg n)
  (unless (<= 0 n 255) (error 'format-256-bg "256 color must be 0-255, got ~a" n))
  (string->bytes/utf-8 (format "\e[48;5;~am" n)))

;; ============================================================
;; RGB 色
;; ============================================================

(define (format-rgb-fg r g b)
  (string->bytes/utf-8 (format "\e[38;2;~a;~a;~am" r g b)))

(define (format-rgb-bg r g b)
  (string->bytes/utf-8 (format "\e[48;2;~a;~a;~am" r g b)))

(define (format-rgb-fg-bg fr fg fb br bg bb)
  (string->bytes/utf-8 (format "\e[38;2;~a;~a;~a;48;2;~a;~a;~am" fr fg fb br bg bb)))

;; ============================================================
;; 颜色/属性构造器
;; ============================================================

(define (color-fg n) (format-fg n))
(define (color-bg n) (format-bg n))
(define (color256-fg n) (format-256-fg n))
(define (color256-bg n) (format-256-bg n))
(define (color-rgb-fg r g b) (format-rgb-fg r g b))
(define (color-rgb-bg r g b) (format-rgb-bg r g b))

(define attr-bold format-bold)
(define attr-dim format-dim)
(define attr-italic format-italic)
(define attr-underline format-underline)
(define attr-blink format-blink)
(define attr-reverse format-reverse)

;; ============================================================
;; 样式系统
;; ============================================================

(define style-registry (make-hash))

(define (style-define! name . specs)
  (hash-set! style-registry name specs))

(define (style-reset)
  format-reset)

(define (style->bytes name)
  (define specs (hash-ref style-registry name
                (lambda () (error 'style->bytes "Undefined style: ~a" name))))
  (apply bytes-append specs))

(define (format-styled name v)
  (bytes-append (style->bytes name) (format-content v) format-reset))

;; ============================================================
;; 格式化属性
;; ============================================================

(define (format-styled-bold v)
  (bytes-append format-bold (format-content v) format-reset))

(define (format-styled-dim v)
  (bytes-append format-dim (format-content v) format-reset))

(define (format-styled-italic v)
  (bytes-append format-italic (format-content v) format-reset))

(define (format-styled-underline v)
  (bytes-append format-underline (format-content v) format-reset))

(define (format-styled-blink v)
  (bytes-append format-blink (format-content v) format-reset))

(define (format-styled-reverse v)
  (bytes-append format-reverse (format-content v) format-reset))
