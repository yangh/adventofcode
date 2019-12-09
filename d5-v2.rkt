#lang racket

(require "intcode.rkt")

(require "input.rkt")

(define input (first (input-load-lines 5)))

(define ic (new Intcode%))

; Part 1
(send ic load-code input)
(send ic set-input 1)
(send ic run)
(send ic display-output)

; Part 2
(send ic load-code input)
(send ic set-input 5)
(send ic run)
(send ic display-output)