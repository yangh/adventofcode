#lang racket
;
; Usage: grep [key-word] [file-pattern]
;
; grep files in the currently directory
;

(define text-filter "set-input")
(define file-filter ".rkt")

(define cmd-options (current-command-line-arguments))

; Command line options
(cond
  [(= (vector-length cmd-options) 1)
   (set! text-filter (vector-ref cmd-options 0))]
  [(= (vector-length cmd-options) 2)
   (set! text-filter (vector-ref cmd-options 0))
   (set! file-filter (vector-ref cmd-options 1))])

(displayln (format "Search text ~a in files ~a" text-filter file-filter))

(define last-line-count 0)

; Grep file
(define (grep file)
  (let ([lines (port->lines (open-input-file file))])
    (set! last-line-count (length lines))
    (foldr append '()
           (map
            (λ (lno line)
              (if (regexp-match text-filter line)
                  (list (format "~a: ~a" lno line))
                  '()))
            (range 1 (add1 (length lines)))
            lines))))

; Match statistics
(define file-count 0)
(define line-count 0)

(define total-line-count 0)

; Iterate files in the current directory
(for-each
 (λ (p)
   (when (and (file-exists? p)
              (regexp-match file-filter (path->string p)))
     (let ([lines (grep p)])
       (set! total-line-count (+ last-line-count total-line-count))
       (when (not (empty? lines))
         (set! file-count (add1 file-count))
         (set! line-count (+ line-count (length lines)))
         (displayln (format "> ~a" (path->string p)))
         (map displayln lines)
         (displayln "")))
     ))
 (directory-list))

(displayln (format "--(~a)--(~a)--" file-filter text-filter))
(displayln (format "File matched: ~a" file-count))
(displayln (format "Line matched: ~a" line-count))
(displayln (format "Total lines: ~a" total-line-count))