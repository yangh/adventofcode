#lang racket
;
; Usage: grep [key-word] [file-pattern]
;
; grep files in currently directory
;

(define txt-filter "set-input")
(define file-filter ".rkt")

(define cmd-options (current-command-line-arguments))

(cond
  [(= (vector-length cmd-options) 1)
   (set! txt-filter (vector-ref cmd-options 0))]
  [(= (vector-length cmd-options) 2)
   (set! txt-filter (vector-ref cmd-options 0))
   (set! file-filter (vector-ref cmd-options 1))])

(displayln (format "Search text ~a in files ~a" txt-filter file-filter))

(define (grep file)
  (let ([lines (port->lines (open-input-file file))])
    (reverse
     (foldl append '()
            (map
             (λ (lno line)
               (if (regexp-match txt-filter line)
                   (list (format "~a:~a" lno line))
                   '()))
             (range 1 (add1 (length lines)))
             lines)))))

(for-each
 (λ (p)
   (when (regexp-match file-filter (path->string p))
     ; Regular file only
     (when (file-exists? p)
       (let ([ret (grep p)])
         (when (> (length ret) 0)
           (displayln (format "> ~a" (path->string p)))
           (map displayln ret)
           (displayln "")))
       )))
 (directory-list))