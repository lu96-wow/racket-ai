#lang racket

;; 导出 ai-dsl.rkt 中定义的所有内容
(require "ai-dsl.rkt")

;; 重新导出，使得 (require ai) 可以访问所有内容
(provide (all-from-out "ai-dsl.rkt"))