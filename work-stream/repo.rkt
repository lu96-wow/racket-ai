#lang racket
;; ============================================================
;; work-stream/repo.rkt — 仓库级工具权限控制
;;
;; 基于 tools/tool.rkt 的权限控制原语，提供命令行友好接口。
;;
;; 功能:
;;   全局模式: 允许/确认（所有工具生效）
;;   单工具模式: 允许/确认（覆盖全局模式）
;;   状态查询: 查看全局 + 每个工具的当前模式
;;
;; 用法 (在 REPL 中集成):
;;   (require "repo.rkt")
;;   (repo-tool-global-confirm)   ;; 所有工具需要确认
;;   (repo-tool-allow "shell")    ;; shell 工具不需要确认
;;   (repo-tool-status)           ;; 显示当前状态
;; ============================================================

(require "../tools/tool.rkt"
         "../format-color/core.rkt"
         "../format-color/styles.rkt")

(provide
 ;; 全局模式控制
 repo-tool-global-allow
 repo-tool-global-confirm
 repo-tool-global-mode?
 ;; 单工具模式控制
 repo-tool-allow
 repo-tool-confirm
 repo-tool-clear
 repo-tool-mode?
 ;; 状态查询
 repo-tool-status
 repo-tool-list-names
 ;; REPL 命令处理
 repo-tool-dispatch-repl)

;; ============================================================
;; 全局模式
;; ============================================================

(define (repo-tool-global-allow)
  "设置全局为「自动允许」模式 — 所有工具直接执行，无需确认"
  (tool-set-global-allow)
  (display clr-green)
  (display "✓ 全局模式已设为「自动允许」— 所有工具将直接执行\n")
  (display format-reset))

(define (repo-tool-global-confirm)
  "设置全局为「需确认」模式 — 所有工具调用前询问用户"
  (tool-set-global-confirm)
  (display clr-yellow)
  (display "✓ 全局模式已设为「需确认」— 工具调用前将询问\n")
  (display format-reset))

(define (repo-tool-global-mode?)
  "返回当前全局模式: 'allow | 'confirm"
  (if (unbox tool-global-mode) 'confirm 'allow))

;; ============================================================
;; 单工具模式
;; ============================================================

(define (repo-tool-allow name)
  "设置指定工具为「自动允许」（覆盖全局模式）"
  (tool-set-allow name)
  (display clr-green)
  (printf "✓ 工具「~a」已设为「自动允许」\n" name)
  (display format-reset))

(define (repo-tool-confirm name)
  "设置指定工具为「需确认」（覆盖全局模式）"
  (tool-set-confirm name)
  (display clr-yellow)
  (printf "✓ 工具「~a」已设为「需确认」\n" name)
  (display format-reset))

(define (repo-tool-clear)
  "清除所有单工具模式，恢复全局模式控制"
  (tool-clear-modes!)
  (display clr-green)
  (display "✓ 已清除所有单工具模式设置，恢复全局模式控制\n")
  (display format-reset))

(define (repo-tool-mode? name)
  "查询指定工具的当前模式: 'allow | 'confirm"
  (if (hash-ref tool-per-modes name #f) 'confirm
      (repo-tool-global-mode?)))

;; ============================================================
;; 状态查询
;; ============================================================

(define (repo-tool-list-names tools)
  "返回工具名列表（若 tools 为 #f 则返回空列表）"
  (if tools (tools-names tools) '()))

(define (repo-tool-status #:tools [tools #f])
  "显示当前工具权限状态"
  (display clr-cyan)
  (display "=== 工具权限状态 ===\n")
  (display format-reset)

  ;; 全局模式
  (define gmode (repo-tool-global-mode?))
  (display "全局模式: ")
  (display (case gmode
             [(allow)   (string-append clr-green "自动允许")]
             [(confirm) (string-append clr-yellow "需确认")]))
  (display format-reset)
  (newline)

  ;; 单工具模式
  (define per-modes (hash-ref tool-per-modes 'keys #f))
  (when (positive? (hash-count tool-per-modes))
    (display "单工具覆盖:\n")
    (for ([(name v) (in-hash tool-per-modes)])
      (printf "  - ~a: " name)
      (display clr-yellow)
      (display "需确认")
      (display format-reset)
      (newline)))

  ;; 显示工具列表
  (when tools
    (display "可用工具:\n")
    (for ([name (in-list (tools-names tools))])
      (define mode (repo-tool-mode? name))
      (printf "  - ~a [" name)
      (case mode
        [(allow)
         (display clr-green)
         (display "自动允许")]
        [(confirm)
         (display clr-yellow)
         (display "需确认")])
      (display format-reset)
      (display "]\n")))

  (display clr-cyan)
  (display "====================\n")
  (display format-reset))

;; ============================================================
;; REPL 命令分发
;; ============================================================

(define (show-tool-help)
  (display clr-cyan)
  (display "工具权限命令:\n")
  (display "  /tool global allow     全局自动允许（所有工具直接执行）\n")
  (display "  /tool global confirm   全局需确认（所有工具调用前询问）\n")
  (display "  /tool allow <name>     设置单个工具为自动允许\n")
  (display "  /tool confirm <name>   设置单个工具为需确认\n")
  (display "  /tool clear            清除所有单工具设置\n")
  (display "  /tool list [<name>]    查看工具模式（可指定单个工具）\n")
  (display "  /tool status           查看完整权限状态\n")
  (display "  /tool help             显示本帮助\n")
  (display format-reset))

(define (repo-tool-dispatch-repl parts #:tools [tools #f])
  "REPL 命令分发: parts 是 (list \"tool\" ...) 的字符串列表
   返回 #t 表示命令已处理，#f 表示未知子命令"
  (match parts
    [(list _ "help")
     (show-tool-help)
     #t]
    [(list _ "global" "allow")
     (repo-tool-global-allow)
     #t]
    [(list _ "global" "confirm")
     (repo-tool-global-confirm)
     #t]
    [(list _ "allow" name)
     (repo-tool-allow name)
     #t]
    [(list _ "confirm" name)
     (repo-tool-confirm name)
     #t]
    [(list _ "clear")
     (repo-tool-clear)
     #t]
    [(list _ "list")
     (if tools
         (begin
           (display "工具权限模式:\n")
           (for ([name (in-list (tools-names tools))])
             (define mode (repo-tool-mode? name))
             (printf "  - ~a [" name)
             (case mode
               [(allow) (display clr-green) (display "自动允许")]
               [(confirm) (display clr-yellow) (display "需确认")])
             (display format-reset)
             (display "]\n")))
         (display "当前未配置工具。\n"))
     #t]
    [(list _ "list" name)
     (define mode (repo-tool-mode? name))
     (printf "工具「~a」当前模式: " name)
     (case mode
       [(allow) (display clr-green) (display "自动允许")]
       [(confirm) (display clr-yellow) (display "需确认")])
     (display format-reset)
     (newline)
     #t]
    [(list _ "status")
     (repo-tool-status #:tools tools)
     #t]
    [_
     #f]))
