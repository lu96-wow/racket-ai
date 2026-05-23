#lang racket

(provide
 tongyi-base-url
 tongyi-chat-endpoint
 tongyi-qwen-turbo tongyi-qwen-plus tongyi-qwen-max
 tongyi-api-key)

(define tongyi-base-url      "https://dashscope.aliyuncs.com")
(define tongyi-chat-endpoint "/compatible-mode/v1/chat/completions")

(define tongyi-qwen-turbo "qwen-turbo")
(define tongyi-qwen-plus  "qwen-plus")
(define tongyi-qwen-max   "qwen-max")

(define (tongyi-api-key)
  (or (getenv "DASHSCOPE_API_KEY")
      (error 'tongyi-api-key "DASHSCOPE_API_KEY env var not set")))
