#lang racket

(define dir (path->string (simplify-path (current-directory))))

(system* (find-executable-path "raco") "pkg" "remove" "ai")
(system* (find-executable-path "raco") "pkg" "install" "--link" "--name" "ai" dir)

(printf "完成！现在可以用 (require ai)\n")