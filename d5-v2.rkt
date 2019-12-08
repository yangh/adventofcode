#lang racket

(require "intcode.rkt")

(require "input.rkt")

(define input (first (input-load-lines 5)))

(define ic (new Intcode%))

(send ic load-code "1,9,10,3,2,3,11,0,99,30,40,50")
(send ic run)

; Part 1
(send ic load-code input)
(send ic set-user-input 1)
(send ic run)

; Part 2
(send ic load-code input)
(send ic set-user-input 5)
(send ic run)