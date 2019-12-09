#lang racket

(require "input.rkt")

(define ms (map string->number (input-load-lines 1)))

(define (fuel m)
  (- (floor (/ m 3)) 2))

(define (cal func inputs)
  (foldl (lambda (a r) (+ r (func a))) 0 inputs))

; Part 1, simply sum all fuel
(displayln (format "Day1 part 1: ~a" (cal fuel ms)))

; Part 2, accumulately sum all fuel
(define (fuel-acc m)
  (define f (fuel m))
  (cond
    [(> f 0)
     (+ f (fuel-acc f))]
    [else 0]))

(displayln (format "Day1 part 2: ~a" (cal fuel-acc ms)))
