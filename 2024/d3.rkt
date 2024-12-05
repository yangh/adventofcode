#lang racket

(require math/base)
(require "utils.rkt")

(define lines (input-load-lines "3"))

(define prog (string-join lines))

(define muls (regexp-match* #rx"mul\\((([0-9]*),([0-9]*))\\)" prog #:match-select values))

; 184122457
(sum (map (lambda (vals)
            (* (string->number (list-ref vals 2))
               (string->number (list-ref vals 3))))
          muls))

(define dodontmuls (regexp-match* #rx"mul\\((([0-9]*),([0-9]*))\\)|do\\(\\)|don't\\(\\)" prog #:match-select values))

(define do #t)

; 107862689
(sum (map (lambda (vals)
            (cond
              ([string=? "do()" (first vals)]
               (set! do #t) 0)
              ([string=? "don't()" (first vals)]
               (set! do #f) 0)
              (else
               ;(displayln vals)
               (if do
                   (* (string->number (list-ref vals 2))
                      (string->number (list-ref vals 3)))
                   0))))
          dodontmuls))