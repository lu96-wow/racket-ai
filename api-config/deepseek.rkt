#lang racket

;; ============================================================
;; api-config/deepseek.rkt — DeepSeek API 配置与常量
;;
;; 引用来源:
;;   https://api-docs.deepseek.com/zh-cn/
;;   https://api-docs.deepseek.com/zh-cn/guides/thinking_mode
;; ============================================================

(provide
 ;; ---- URL ----
 deepseek-base-url
 deepseek-chat-endpoint

 ;; ---- 模型 ----
 deepseek-v4-flash
 deepseek-v4-pro

 ;; ---- 认证 ----
 deepseek-api-key)

;; ============================================================
;; URL
;; ============================================================

(define deepseek-base-url           "https://api.deepseek.com")
(define deepseek-chat-endpoint      "/chat/completions")

;; ============================================================
;; 模型
;; ============================================================

(define deepseek-v4-flash  "deepseek-v4-flash")
(define deepseek-v4-pro    "deepseek-v4-pro")
;; ============================================================
;; 认证
;; ============================================================

(define (deepseek-api-key)
  (or (getenv "DEEPSEEK_API_KEY")
      (error 'deepseek-api-key "DEEPSEEK_API_KEY env var not set")))

