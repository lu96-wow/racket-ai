#lang racket

;; ============================================================
;; deepseek-chat.rkt -- Wan Zheng AI Dui Hua Liu Cheng
;;
;; Ji yu env/ + history/, ti gong:
;;   - hui hua guan li (li shi ji lu)
;;   - zi dong gong ju xun huan
;;   - fen zhi / zhong sheng
;;   - liu shi yu fei liu shi
;;
;; Zu He Shi She Ji:
;;   hui hua shi ke jian cha de shu ju jie gou,
;;   bu shi feng bi de kuang jia.
;; ============================================================

(require "format-color/core.rkt"
         "format-color/styles.rkt"
         "env/deepseek-env.rkt"
         "api-config/deepseek.rkt"
         "tools/deepseek-base-tool.rkt"
         "history/history.rkt")

(provide
 ;; ---- gou jian ----
 make-chat-session
 chat-session?

 ;; ---- dui hua ----
 chat!                  ; chat-session? string? -> string?
 chat-stream!           ; chat-session? string? -> string?

 ;; ---- gong ju xun huan (fei liu shi) ----
 chat-loop!             ; chat-session? string? -> string?

 ;; ---- li shi cao zuo ----
 chat-retry!            ; chat-session? -> string?
 chat-branch!           ; chat-session? string? -> string?
 chat-rewind!           ; chat-session? -> chat-session?

 ;; ---- shu gong ju cao zuo (fei xian xing) ----
 chat-continue!         ; chat-session? [h-node?] string? -> chat-session?
                        ;   cong zhi ding jie dian ji xu (zi dong fen zhi)
 chat-retry-node!       ; chat-session? [h-node?] -> chat-session?
                        ;   cong zhi ding jie dian zhong pao (xiang tong shu ru)
 chat-squash!           ; chat-session? h-node? h-node? -> chat-session?
                        ;   AI zong jie start→end dui hua, ti huan wei yi ge jie dian
 chat-re-root!          ; chat-session? [h-node?] -> chat-session?
                        ;   yi jie dian wei gen zhong jian li xin shu

 ;; ---- cha xun ----
 chat-history           ; chat-session? -> (listof hash?)
 chat-last-response     ; chat-session? -> (or/c string? #f)
 chat-session-info      ; chat-session? -> void?
 )

;; ============================================================
;; Chat Session Jie Gou Ti
;; ============================================================

(struct chat-session (env        ; env?
                      history    ; h-root?
                      current    ; h-node? -- dang qian wei zhi
                      tools)     ; tools? | #f
  #:transparent)

;; ============================================================
;; make-chat-session : -> chat-session?
;;
;; Chuang jian yi ge xin de hui hua hui hua.
;;
;; can shu:
;;   #:model      -- mo xing (mo ren: deepseek-v4-flash)
;;   #:max-tokens -- zui da token shu (mo ren: 4096)
;;   #:tools      -- gong ju ji (mo ren: default-tools)
;;   #:on-content -- liu shi hui diao (mo ren: display)
;; ============================================================

(define (make-chat-session
         #:model [model deepseek-v4-flash]
         #:max-tokens [max-tokens 4096]
         #:tools [tools default-tools]
         #:on-content [on-content (lambda (c) (display c) (flush-output))])
  (define env
    (make-env #:model model
              #:max-tokens max-tokens
              #:tools tools
              #:on-content on-content))
  (define root (make-root))
  (chat-session env root (h-root-node root) tools))

;; ============================================================
;; Nei Bu: Jiang li shi jie dian zhuan huan wei API xiao xi lie biao
;; ============================================================

(define (node->messages node)
  (define path (h-path node))
  ;; tiao guo root (first), cong di er ge kai shi
  (define pairs
    (for/list ([n (in-list (cdr path))])
      (define user (h-node-user-content n))
      (define ai   (h-node-ai-content n))
      (cons user ai)))
  (define result '())
  (for ([p (in-list pairs)])
    (define user-msg (build-user-message #:content (car p)))
    (set! result (append result (build-messages user-msg)))
    (define ai-content (cdr p))
    (when ai-content
      (if (string? ai-content)
          (set! result (append result
            (build-messages (build-assistant-message #:content ai-content))))
          (set! result (append result
            (build-messages (build-assistant-message
              #:content (hash-ref ai-content (quote content) "")
              #:tool_calls (hash-ref ai-content (quote tool_calls) #f))))))))
  result)

;; ============================================================
;; chat! : Fei Liu Shi Dui Hua (dan lun, bu han gong ju xun huan)
;;
;; fa song xiao xi, deng dai wan zheng xiang ying, fan hui nei rong.
;; ru guo you gong ju diao yong, fan hui xiang ying yuan shi nei rong.
;; ============================================================

(define (chat! session msg)
  (define msgs (node->messages (chat-session-current session)))
  (define new-msgs
    (append msgs (build-messages (build-user-message #:content msg))))
  (define resp (env-chat (chat-session-env session) new-msgs))
  (define content (response-content resp))
  (define tcs (response-tool-calls resp))

  ;; ji lu li shi
  (define root (chat-session-history session))
  (define cur (chat-session-current session))
  (define-values (new-root new-node)
    (h-next root cur msg))
  (define ai-saved
    (if tcs
        (hasheq 'content content 'tool_calls tcs)
        (or content "")))
  (h-set-ai! new-root new-node ai-saved)

  (struct-copy chat-session session
    [history new-root]
    [current new-node]))

;; ============================================================
;; chat-loop! : Fei Liu Shi + Zi Dong Gong Ju Xun Huan
;;
;; fa song xiao xi, zi dong zhi xing gong ju, fan hui zui zhong wen ben.
;; ============================================================

(define (chat-loop! session msg)
  (define msgs (node->messages (chat-session-current session)))
  (define env (chat-session-env session))
  (define tools (chat-session-tools session))
  (define root (chat-session-history session))
  (define cur (chat-session-current session))

  ;; xian ji lu yong hu xiao xi
  (define-values (r1 n1) (h-next root cur msg))

  (let loop ([msgs (append msgs (build-messages (build-user-message #:content msg)))]
             [node n1]
             [root r1]
             [n 0])
    (if (>= n 10)
        (begin
          (printf "\n[da dao zui da lun shu 10]\n")
          (struct-copy chat-session session
            [history root] [current node]))
        (let ()
          (define resp (env-chat env msgs))
          (define content (response-content resp))
          (define tcs (response-tool-calls resp))

          (cond
            ;; wu gong ju -- wan cheng
            [(not tcs)
             (h-set-ai! root node (or content ""))
             (struct-copy chat-session session
               [history root] [current node])]

            ;; you gong ju -- zhi xing bing ji xu
            [else
             (define ai-saved
               (hasheq 'content (or content "") 'tool_calls tcs))
             (h-set-ai! root node ai-saved)
             (define new-msgs (tool-chat-request tools resp msgs))
             (define-values (r2 n2) (h-next root node
               (format "[gong ju zhi xing jie guo: ~a ge gong ju]" (length tcs))))
             (h-set-ai! r2 n2
               (format "zhi xing le ~a ge gong ju" (length tcs)))
             (loop new-msgs n2 r2 (add1 n))])))))

;; ============================================================
;; chat-stream! : Liu Shi Dui Hua
;;
;; zhi chi liu shi shu chu + gong ju xun huan.
;; ============================================================

(define (chat-stream! session msg)
  (define msgs (node->messages (chat-session-current session)))
  (define env (chat-session-env session))
  (define tools (chat-session-tools session))
  (define root (chat-session-history session))
  (define cur (chat-session-current session))

  (define-values (r1 n1) (h-next root cur msg))

  (let loop ([msgs (append msgs (build-messages (build-user-message #:content msg)))]
             [node n1]
             [root r1]
             [n 0])
    (if (>= n 10)
        (begin
          (printf "\n[da dao zui da lun shu 10]\n")
          (struct-copy chat-session session
            [history root] [current node]))
        (let ()
          (define acc "")
          (define tc-hash (hasheq))
          (define content-cb (lambda (c) (set! acc (string-append acc c))))
          (define tc-cb (lambda (tcs) (set! tc-hash (merge-tool-calls tc-hash tcs))))

          (env-chat/stream env msgs
            #:on-content content-cb
            #:on-tool-calls tc-cb)

          (define sorted-keys (sort (hash-keys tc-hash) <))
          (define tool-calls
            (for/list ([k (in-list sorted-keys)])
              (hash-ref tc-hash k)))

          (cond
            [(null? tool-calls)
             (h-set-ai! root node acc)
             (struct-copy chat-session session
               [history root] [current node])]
            [else
             (define ai-saved
               (hasheq 'content acc 'tool_calls tool-calls))
             (h-set-ai! root node ai-saved)
             (define new-msgs (tool-chat-request tools
               (hasheq 'choices
                 (list (hasheq 'message
                   (hasheq 'role "assistant"
                           'content acc
                           'tool_calls tool-calls))))
               msgs))
             (define-values (r2 n2) (h-next root node
               (format "[gong ju zhi xing: ~a ge]" (length tool-calls))))
             (h-set-ai! r2 n2
               (format "zhi xing le ~a ge gong ju" (length tool-calls)))
             (loop new-msgs n2 r2 (add1 n))])))))

;; ============================================================
;; chat-retry! : Zhong Sheng Zui Hou Yi Tiao Hui Fu
;; ============================================================

(define (chat-retry! session)
  (define cur (chat-session-current session))
  (define parent (h-node-parent cur))
  (unless parent
    (error 'chat-retry! "mei you shang yi tiao xiao xi"))
  (define user-msg (h-node-user-content cur))
  ;; shan chu dang qian fen zhi, chuang jian xin fen zhi
  (define root (chat-session-history session))
  (define new-node (h-branch-same root parent))
  ;; xin fen zhi wu AI nei rong, yong hu xiao xi tong parent
  ;; zhi jie yong chat! fa song xiang tong nei rong
  (chat! (struct-copy chat-session session
           [history root] [current new-node])
         user-msg))

;; ============================================================
;; chat-branch! : Fen Zhi Dui Hua
;; ============================================================

(define (chat-branch! session msg)
  (define root (chat-session-history session))
  (define cur (chat-session-current session))
  (define-values (new-root new-node)
    (h-branch-new root cur msg))
  (chat! (struct-copy chat-session session
           [history new-root] [current new-node])
         msg))

;; ============================================================
;; chat-rewind! : Tui Hui Dao Shang Yi Tiao
;; ============================================================

(define (chat-rewind! session)
  (define cur (chat-session-current session))
  (define parent (h-node-parent cur))
  (unless parent
    (error 'chat-rewind! "yi jing zai gen jie dian"))
  (struct-copy chat-session session
    [current parent]))

;; ============================================================
;; chat-continue! : Cong Zhi Ding Jie Dian Ji Xu (Zi Dong Fen Zhi)
;;
;; cong `node` (mo ren = dang qian) ji xu dui hua.
;; ru guo `node` yi you zi jie dian, zi dong chuang jian xin fen zhi.
;; ============================================================

(define (chat-continue! session [node (chat-session-current session)]
                       #:msg [msg #f])
  (unless msg
    (error 'chat-continue! "xu yao ti gong xiao xi (msg)"))
  (define root (chat-session-history session))
  (define env (chat-session-env session))
  (define tools (chat-session-tools session))

  (define-values (new-root new-node)
    (h-continue root node msg))

  ;; fa song xiao xi gei AI
  (define msgs (node->messages node))
  (define new-msgs
    (append msgs (build-messages (build-user-message #:content msg))))
  (define resp (env-chat env new-msgs))
  (define content (response-content resp))
  (define tcs (response-tool-calls resp))

  (define ai-saved
    (if tcs
        (hasheq 'content content 'tool_calls tcs)
        (or content "")))
  (h-set-ai! new-root new-node ai-saved)

  (struct-copy chat-session session
    [history new-root]
    [current new-node]))

;; ============================================================
;; chat-retry-node! : Cong Zhi Ding Jie Dian Zhong Pao
;;
;; zai `node` (mo ren = dang qian) de fu jie dian xia chuang jian
;; yi ge xin xiong di jie dian (xiang tong yong hu shu ru), ran hou
;; chong xin fa song gei AI.
;; ============================================================

(define (chat-retry-node! session [node (chat-session-current session)])
  (define root (chat-session-history session))
  (define parent (h-node-parent node))
  (unless parent
    (error 'chat-retry-node! "gen jie dian wu fa zhong pao"))

  (define user-msg (h-node-user-content node))
  (define new-node (h-retry-from root node))

  ;; fa song xiang tong yong hu shu ru gei AI
  (define env (chat-session-env session))
  (define msgs (node->messages parent))
  (define new-msgs
    (append msgs (build-messages (build-user-message #:content user-msg))))
  (define resp (env-chat env new-msgs))
  (define content (response-content resp))
  (define tcs (response-tool-calls resp))

  (define ai-saved
    (if tcs
        (hasheq 'content content 'tool_calls tcs)
        (or content "")))
  (h-set-ai! root new-node ai-saved)

  (struct-copy chat-session session
    [history root]
    [current new-node]))

;; ============================================================
;; chat-squash! : AI Zong Jie Dui Hua, Ti Huan Wei Yi Ge Jie Dian
;;
;; xuan ding start-node he end-node, AI zong jie gai dui hua duan,
;; ran hou yong yi ge xin jie dian qu dai zheng tiao lian.
;; xin jie dian de zi jie dian = end-node de zi jie dian.
;; ============================================================

(define (chat-squash! session start-node end-node)
  (define root (chat-session-history session))
  (define env (chat-session-env session))

  ;; 1. cao zuo li shi shu: shan chu fan wei, fan hui pairs
  (define-values (new-root new-node pairs)
    (h-squash-range root start-node end-node))

  ;; 2. gou jian zong jie prompt
  (define summary-prompt
    (string-append
     "以下是一段对话片段，请用简洁的语言总结这段对话的核心内容。\n\n"
     (string-join
      (for/list ([p (in-list pairs)])
        (format "用户: ~a\nAI: ~a\n"
                (or (car p) "(wu)")
                (cond
                  [(string? (cdr p)) (cdr p)]
                  [(hash? (cdr p)) (hash-ref (cdr p) 'content "(gong ju diao yong)")]
                  [else "(wu)"])))
      "\n")
     "\n\n请用一段话总结以上对话的核心内容和关键信息："))

  ;; 3. qing qiu AI zong jie
  (define summary-msg
    (build-messages (build-user-message #:content summary-prompt)))
  (define resp (env-chat env summary-msg))
  (define summary (or (response-content resp) ""))

  ;; 4. tian chong AI nei rong
  (h-set-ai! new-root new-node summary)

  (struct-copy chat-session session
    [history new-root]
    [current new-node]))

;; ============================================================
;; chat-re-root! : Yi Jie Dian Wei Gen Chong Jian Xin Shu
;;
;; xuan ding yi ge jie dian cheng wei xin de gen jie dian.
;; suo you zi jie dian zi dong ji cheng, xian zu bei qi yong.
;; dang qian jie dian zi dong tiao zheng wei xin shu de gen zi jie dian.
;; ============================================================

(define (chat-re-root! session [node (chat-session-current session)])
  (define root (chat-session-history session))
  (define new-root (h-re-root root node))

  ;; dang qian jie dian: bao chi wei xin shu zhong de tong yi jie dian
  (struct-copy chat-session session
    [history new-root]
    [current node]))

;; ============================================================
;; chat-history : Huo Qu Xiao Xi Lie Biao (gong API shi yong)
;; ============================================================

(define (chat-history session)
  (node->messages (chat-session-current session)))

;; ============================================================
;; chat-last-response : Huo Qu Zui Hou Yi Tiao Hui Fu
;; ============================================================

(define (chat-last-response session)
  (define ai (h-node-ai-content (chat-session-current session)))
  (cond
    [(not ai) #f]
    [(string? ai) ai]
    [(hash? ai) (hash-ref ai (quote content) #f)]
    [else (format "~a" ai)]))

;; ============================================================
;; chat-session-info : Da Yin Hui Hua Xin Xi
;; ============================================================

(define (chat-session-info session)
  (display clr-cyan)
  (display "=== Chat Session ===")
  (display format-reset)
  (newline)
  (display "  Mo Xing:  ")
  (display (env-model (chat-session-env session)))
  (newline)
  (display "  Li Shi:   ")
  (display (length (h-path (chat-session-current session))))
  (display " tiao xiao xi")
  (newline)
  (display "  Ye Zi:    ")
  (display (length (h-leaves (chat-session-history session))))
  (display " ge")
  (newline)
  (display "  Gong Ju:  ")
  (display clr-green)
  (display (if (chat-session-tools session)
               (string-join (tools-names (chat-session-tools session)) ", ")
               "(wu)"))
  (display format-reset)
  (newline)
  (display clr-cyan)
  (display "================")
  (display format-reset)
  (newline))
