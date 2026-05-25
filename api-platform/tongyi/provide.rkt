#lang racket
;; ============================================================
;; api-platform/tongyi/provide.rkt
;; Tongyi/Qwen 平台统一导出接口（带 tongyi- 前缀）
;; ============================================================

(require "chat.rkt"
         "json-build-parse.rkt")

(provide
 tongyi-chat
 tongyi-chat/stream
 (rename-out
  ;; 消息构建
  [build-messages tongyi-build-messages]
  [build-user-message tongyi-build-user-message]
  [build-assistant-message tongyi-build-assistant-message]
  [build-tool-result tongyi-build-tool-result]

  ;; 工具构建
  [build-tool tongyi-build-tool]

  ;; 可用参数键
  [chat-request-keys tongyi-chat-request-keys]
  [message-keys tongyi-message-keys]

  ;; 请求构建
  [build-chat-request tongyi-build-chat-request]

  ;; JSON 转换
  [json->string tongyi-json->string]
  [string->json tongyi-string->json]

  ;; 响应解析
  [response-id tongyi-response-id]
  [response-model tongyi-response-model]
  [response-usage tongyi-response-usage]
  [response-first-choice tongyi-response-first-choice]
  [response-content tongyi-response-content]
  [response-tool-calls tongyi-response-tool-calls]

  ;; Delta 解析
  [choice-delta tongyi-choice-delta]
  [delta-content tongyi-delta-content]
  [delta-tool-calls tongyi-delta-tool-calls]

  ;; 工具调用解析
  [tool-call-id tongyi-tool-call-id]
  [tool-call-func-name tongyi-tool-call-func-name]
  [tool-call-func-args tongyi-tool-call-func-args]))