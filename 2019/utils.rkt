#lang racket

(provide (all-defined-out))

; Load input as list of lines
(define (input-load-lines d)
  (port->lines (open-input-file (format "inputs/d~a.txt" d))))

(define dbg #f)
(define (debug-set! d) (set! dbg d))

(define (ddisplayln msg)
  (when dbg
    (displayln msg)))
