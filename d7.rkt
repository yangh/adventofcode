#lang racket

(require "intcode.rkt")
(require "input.rkt")

(define input (first (input-load-lines 7)))

(define (test input amps)
  (define ic (new Intcode%))
  (define o 0)

  (for-each
   (lambda (a)
     (send ic load-code input)
     (send ic set-input o)
     (send ic set-input a)
     (send ic run)
     ;(send ic display-output)
     (set! o (send ic get-output)))
   (map string->number (string-split amps ",")))
  o)

;(test "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" "4,3,2,1,0")

(define (is-all-diff? a b c d e)
  (define v (make-hash))
  (hash-set! v a a)
  (hash-set! v b b)
  (hash-set! v c c)
  (hash-set! v d d)
  (hash-set! v e e)
  (= (hash-count v) 5))

(define (part1 input)
  (define m 0)
  (define amps "")
  (for* ([a (range 0 5)]
         [b (range 0 5)]
         [c (range 0 5)]
         [d (range 0 5)]
         [e (range 0 5)])
    ;Each pharse used exactly once
    (when (is-all-diff? a b c d e)
      (let* ([as (format "~a,~a,~a,~a,~a" a b c d e)]
             [n (test input as)])
        (when (> n m)
          (set! m n)
          (set! amps as)
          ;(displayln n)
          ))))
  (displayln (format "Max thruster: ~a from ~a" m amps))
  m)

(part1 "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
(part1 "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")

(part1 input)