#lang racket

(require "input.rkt")

(define masses (map string->number (input-load-lines 1)))

(define (fuel mass)
  (- (floor (/ mass 3)) 2))

(define (cal func inputs)
  (foldl (lambda (m total) (+ total (func m))) 0 inputs))

; Part 1, simply sum all fuel
(displayln (format "Day1 part 1: ~a" (cal fuel masses)))

; Part 2, accumulately sum all fuel
(define (fuel-acc mass)
  (define f (fuel mass))
  (cond
    [(<= f 0) 0]
    [else
     (+ f (fuel-acc f))]))

(displayln (format "Day1 part 2: ~a" (cal fuel-acc masses)))
