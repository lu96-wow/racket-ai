#lang racket
;; ============================================================
;; api-platform/deepseek/provide.rkt
;; DeepSeek 平台统一导出接口（带 deepseek- 前缀）
;; ============================================================

(require "chat.rkt"
         "json-build-parse.rkt")

(provide
 deepseek-chat
 deepseek-chat/stream
 (rename-out
  ;; 消息构建
  [build-messages deepseek-build-messages]
  [build-user-message deepseek-build-user-message]
  [build-assistant-message deepseek-build-assistant-message]
  [build-tool-result deepseek-build-tool-result]

  ;; 工具构建
  [build-tool deepseek-build-tool]
  [build-thinking deepseek-build-thinking]

  ;; 可用参数键
  [chat-request-keys deepseek-chat-request-keys]
  [message-keys deepseek-message-keys]

  ;; 请求构建
  [build-chat-request deepseek-build-chat-request]

  ;; JSON 转换
  [json->string deepseek-json->string]
  [string->json deepseek-string->json]

  ;; 响应解析
  [response-id deepseek-response-id]
  [response-model deepseek-response-model]
  [response-usage deepseek-response-usage]
  [response-first-choice deepseek-response-first-choice]
  [response-content deepseek-response-content]
  [response-tool-calls deepseek-response-tool-calls]

  ;; Delta 解析
  [choice-delta deepseek-choice-delta]
  [delta-content deepseek-delta-content]
  [delta-reasoning deepseek-delta-reasoning]
  [delta-tool-calls deepseek-delta-tool-calls]

  ;; 工具调用解析
  [tool-call-id deepseek-tool-call-id]
  [tool-call-func-name deepseek-tool-call-func-name]
  [tool-call-func-args deepseek-tool-call-func-args]))