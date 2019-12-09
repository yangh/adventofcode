#lang racket

(require "intcode.rkt")
(require "input.rkt")

(define input (first (input-load-lines 9)))

(define (test input)
  (define ic (new Intcode%))

  (send ic set-debug #t)
  (send ic load-code input)
  (send ic run))

;(test "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
;(test "1102,34915192,34915192,7,4,7,99,0")
;(test "104,1125899906842624,99")

(define (part1 input n)
  (define ic (new Intcode%))
  ;(send ic set-debug #t)
  (send ic load-code input)
  (send ic set-input n)
  (send ic run)
  (send ic display-output))

(part1 input 1)
(part1 input 2)