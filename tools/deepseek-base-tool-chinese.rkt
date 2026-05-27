#lang racket
;; ============================================================
;; tools/deepseek-base-tool-chinese.rkt — 内置工具定义（中文描述版）
;;
;; 包含 shell / read / write 三个具体工具（所有描述字段均为中文），
;; 以及由它们组成的默认工具集 default-tools。
;; 同时 re-export tool.rkt 的所有接口。
;; ============================================================

(require "tool.rkt"
         "../api-platform/deepseek/json-build-parse.rkt")

(provide
 ;; re-export 抽象框架
 make-tool build-tools tools-add tools-delete tools-schemas tools-names
 tool-dispatch
 tool-set-global-allow tool-set-global-confirm tool-global-mode
 tool-set-allow tool-set-confirm tool-clear-modes!
 merge-tool-calls
 ;; 内置工具
 shell-tool read-file-tool write-file-tool default-tools)

;; ============================================================
;; 路径安全检查（通用）
;; ============================================================

(define (resolve-path fp)
  "将路径基于当前工作目录解析为规范化绝对路径"
  (path->string (simplify-path (path->complete-path fp))))

(define (is-subpath? parent child)
  "判断 child 是否在 parent 目录下"
  (define p (path->string (simplify-path parent)))
  (define c (path->string (simplify-path child)))
  (and (>= (string-length c) (string-length p))
       (string=? (substring c 0 (string-length p)) p)))

(define (check-path-escape fp)
  "检查路径是否安全（在当前工作目录下）。安全返回 #f，不安全返回错误消息。"
  (define cwd (current-directory))
  (with-handlers ([exn:fail? (lambda (e) (format "路径解析失败: ~a" (exn-message e)))])
    (define resolved (resolve-path fp))
    (if (is-subpath? cwd resolved)
        #f
        (format "路径逃逸: ~a 不在当前工作目录 ~a 内" resolved (path->string cwd)))))

;; ============================================================
;; Shell
;; ============================================================

(define (run-shell-command cmd)
  (with-handlers ([exn:fail? (lambda (e) (format "Error: ~a" (exn-message e)))])
    (define out (open-output-string))
    (define err (open-output-string))
    (parameterize ([current-output-port out] [current-error-port err]) (system cmd))
    (define s (string-trim (get-output-string out)))
    (define e (string-trim (get-output-string err)))
    (define combined (string-append s (if (positive? (string-length e)) (format "\n[stderr] ~a" e) "")))
    (if (equal? combined "") "(no output)" combined)))

(define (run-shell/hash a) (run-shell-command (hash-ref a '命令 "")))
(define (security-shell a) #f)

(define shell-tool
  (make-tool "shell"
             (build-tool #:name "shell" #:description "执行一条 shell 命令。"
                         #:parameters
                         (hasheq 'type "object" 'properties
                                 (hasheq '命令 (hasheq 'type "string" 'description "要执行的 shell 命令"))
                                 'required (list "命令")))
             #:run run-shell/hash #:security security-shell))


;; ============================================================
;; Read
;; ============================================================

(define (file->lines p) (string-split (file->string p) "\n"))

(define (run-read-file fp sl el)
  (with-handlers ([exn:fail? (lambda (e) (format "Error: ~a" (exn-message e)))])
    (define ls (file->lines fp))
    (define from (max 1 (or sl 1)))
    (define to   (min (length ls) (or el (length ls))))
    (string-join (for/list ([i (in-range (sub1 from) to)]) (list-ref ls i)) "\n")))

(define (run-read/hash a)
  (run-read-file (hash-ref a '文件路径) (hash-ref a '起始行 #f) (hash-ref a '结束行 #f)))

(define (security-read a)
  (check-path-escape (hash-ref a '文件路径 "")))

(define read-file-tool
  (make-tool "read_file"
             (build-tool #:name "read_file" #:description "读取一个文件。"
                         #:parameters
                         (hasheq 'type "object" 'properties
                                 (hasheq '文件路径   (hasheq 'type "string"  'description "文件路径")
                                         '起始行 (hasheq 'type "integer" 'description "起始行（可选）")
                                         '结束行   (hasheq 'type "integer" 'description "结束行（可选）"))
                                 'required (list "文件路径")))
             #:run run-read/hash #:security security-read))


;; ============================================================
;; Write（含安全检查：路径逃逸检测）
;; ============================================================

(define (run-write-file fp ct sl el)
  (with-handlers ([exn:fail? (lambda (e) (format "Error: ~a" (exn-message e)))])
    (unless fp (error "filepath required")) (unless ct (error "content required"))
    (if (not sl)
        (begin (display-to-file ct fp #:exists 'replace) "ok")
        (let* ([all (if (file-exists? fp) (file->lines fp) '())]
               [start (max 1 sl)] [end (min (length all) (or el (length all)))]
               [new (string-split ct "\n")])
          (display-to-file (string-join (append (take all (sub1 start)) new (drop all end)) "\n")
                           fp #:exists 'replace)
          "ok"))))

(define (run-write/hash a)
  (run-write-file (hash-ref a '文件路径 #f) (hash-ref a '内容 #f)
                  (hash-ref a '起始行 #f) (hash-ref a '结束行 #f)))

(define (security-write a)
  (check-path-escape (hash-ref a '文件路径 "")))

(define write-file-tool
  (make-tool "write_file"
             (build-tool #:name "write_file" #:description "写入一个文件。"
                         #:parameters
                         (hasheq 'type "object" 'properties
                                 (hasheq '文件路径   (hasheq 'type "string"  'description "文件路径")
                                         '内容    (hasheq 'type "string"  'description "文件内容")
                                         '起始行 (hasheq 'type "integer" 'description "起始行（可选）")
                                         '结束行   (hasheq 'type "integer" 'description "结束行（可选）"))
                                 'required (list "文件路径" "内容")))
             #:run run-write/hash #:security security-write))

;; ============================================================
;; 默认工具集
;; ============================================================

(define default-tools (build-tools shell-tool read-file-tool write-file-tool))
