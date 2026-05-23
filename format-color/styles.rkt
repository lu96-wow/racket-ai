#lang racket
;; ============================================================
;; format-color/styles.rkt — 预定义颜色/属性常量
;;。
;; ============================================================

(require "core.rkt")

(provide
 clr-black clr-red clr-green clr-yellow
 clr-blue clr-magenta clr-cyan clr-white clr-default
 bclr-black bclr-red bclr-green bclr-yellow
 bclr-blue bclr-magenta bclr-cyan bclr-white bclr-default)

(define clr-black   (color-fg 0))
(define clr-red     (color-fg 1))
(define clr-green   (color-fg 2))
(define clr-yellow  (color-fg 3))
(define clr-blue    (color-fg 4))
(define clr-magenta (color-fg 5))
(define clr-cyan    (color-fg 6))
(define clr-white   (color-fg 7))
(define clr-default (color-fg 9))

(define bclr-black   (color-bg 0))
(define bclr-red     (color-bg 1))
(define bclr-green   (color-bg 2))
(define bclr-yellow  (color-bg 3))
(define bclr-blue    (color-bg 4))
(define bclr-magenta (color-bg 5))
(define bclr-cyan    (color-bg 6))
(define bclr-white   (color-bg 7))
(define bclr-default (color-bg 9))
