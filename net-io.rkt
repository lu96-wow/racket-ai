#lang racket

;; ============================================================
;; net-io.rkt — 纯 HTTP JSON 数据流传输层
;;
;; 不包含任何平台相关代码。
;; 提供: HTTP POST、SSE 流读取（可中断、支持无限长度 JSON）
;; ============================================================

(require net/http-client
         net/url
         json)

(provide
 ;; ---- 通用 HTTP ----
 http-post                    ; url headers body-str -> (values status headers body-port)

 ;; ---- SSE 读取 (供 chat.rkt re-export) ----
 in-sse

 ;; ---- 辅助 ----
 http-error?
 read-response-body)

;; ============================================================
;; URL 解析
;; ============================================================

(define (url-path->string path)
  (if (null? path)
      "/"
      (string-join
       (map (lambda (seg) (if (string? seg) seg (path/param-path seg)))
            path)
       "/" #:before-first "/")))

(define (parse-url str)
  (define u (string->url str))
  (values (url-host u)
          (or (url-port u) (if (string=? (url-scheme u) "https") 443 80))
          (url-path->string (url-path u))
          (string=? (url-scheme u) "https")))

;; ============================================================
;; POST 原语
;; ============================================================

(define (http-post url headers body-str)
  (define-values (host port path ssl?) (parse-url url))
  (define body-bytes (string->bytes/utf-8 body-str))
  (http-sendrecv host path
                 #:ssl? (and ssl? 'auto)
                 #:port port
                 #:method "POST"
                 #:data body-bytes
                 #:headers headers))

;; ============================================================
;; SSE 读取
;;
;; read-sse-chunk 返回 (values status data):
;;   'ok    + jsexpr   — 正常数据块
;;   'done  + #f       — 正常结束 ([DONE])
;;   'error + string   — 异常结束 (EOF 未收到 [DONE])
;;
;; in-sse 仅在 'ok 时产出数据，'done 自然结束，'error 抛异常。
;; ============================================================

(define (read-sse-chunk port)
  (let loop ()
    (define line (read-line port))
    (cond
      [(eof-object? line) (values 'error "stream closed before [DONE]")]
      [(string-prefix? line "data: ")
       (define data (substring line 6))
       (if (string=? data "[DONE]")
           (values 'done #f)
           (values 'ok (string->jsexpr data)))]
      [else (loop)])))

(define (in-sse port #:stop? [stop? (lambda () #f)])
  (define stopped? #f)
  (in-producer (lambda ()
                 (cond
                   [stopped? eof]
                   [(stop?)
                    (set! stopped? #t)
                    eof]
                   [else
                    (let-values ([(status data) (read-sse-chunk port)])
                      (case status
                        [(ok) data]
                        [(done) (set! stopped? #t) eof]
                        [(error) (raise (make-exn:fail
                                         (format "SSE error: ~a" data)
                                         (current-continuation-marks)))]))]))
               eof))

;; ============================================================
;; 辅助
;; ============================================================

(define (http-error? status headers body-port)
  (not (string-prefix? (bytes->string/utf-8 status) "HTTP/1.1 2")))

(define (read-response-body port)
  (define out (open-output-string))
  (copy-port port out)
  (get-output-string out))