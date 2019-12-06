#lang racket

(provide (all-defined-out))

(define (input-load-lines d)
  (port->lines (open-input-file (format "inputs/~a.txt" d))))
