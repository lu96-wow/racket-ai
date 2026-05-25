#lang racket

(provide
 tongyi-base-url
 tongyi-chat-endpoint
 qwen-flash
 tongyi-api-key)

(define tongyi-base-url      "https://dashscope.aliyuncs.com")
(define tongyi-chat-endpoint "/compatible-mode/v1/chat/completions")

(define qwen-flash "qwen3.5-flash")

(define (tongyi-api-key)
  (or (getenv "DASHSCOPE_API_KEY")
      (error 'tongyi-api-key "DASHSCOPE_API_KEY env var not set")))
