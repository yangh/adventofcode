#lang racket
;
; Usage: grep [key-word] [file-pattern]
;
; grep files in the currently directory
;

(define txt-filter "set-input")
(define file-filter ".rkt")

(define cmd-options (current-command-line-arguments))

; Command line options
(cond
  [(= (vector-length cmd-options) 1)
   (set! txt-filter (vector-ref cmd-options 0))]
  [(= (vector-length cmd-options) 2)
   (set! txt-filter (vector-ref cmd-options 0))
   (set! file-filter (vector-ref cmd-options 1))])

(displayln (format "Search text ~a in files ~a" txt-filter file-filter))

; Grep file
(define (grep file)
  (let ([lines (port->lines (open-input-file file))])
    (foldr append '()
           (map
            (λ (lno line)
              (if (regexp-match txt-filter line)
                  (list (format "~a: ~a" lno line))
                  '()))
            (range 1 (add1 (length lines)))
            lines))))

; Iterate files in the current directory
(for-each
 (λ (p)
   (when (and (file-exists? p)
              (regexp-match file-filter (path->string p)))
     (let ([lines (grep p)])
       (when (not (empty? lines))
         (displayln (format "> ~a" (path->string p)))
         (map displayln lines)
         (displayln "")))
     ))
 (directory-list))
