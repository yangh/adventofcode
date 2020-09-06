#lang racket

(require "input.rkt")

(define input (first (input-load-lines 2)))

(define (input->vector input) (list->vector
                               (map string->number
                                    (string-split input ","))))

;(displayln (format "Code length: ~a" (vector-length ins)))

(define (value-at ints pos)
  (vector-ref ints (vector-ref ints pos)))

(define (op3 op ram pc)
  (vector-set! ram
               (vector-ref ram (+ pc 3))
               (op (value-at ram (+ pc 1))
                   (value-at ram (+ pc 2)))))

(define ADD  1)
(define MULT 2)
(define HALT 99)

(define (intcode ram pc)
  (when (< pc (vector-length ram))
    ;(displayln (format "Pos: ~a" pos))
    (let ([op (vector-ref ram pc)])
      (cond
        [(= op HALT) #f]
        [(= op ADD)
         (op3 + ram pc)
         (intcode ram (+ pc 4))]
        [(= op MULT)
         (op3 * ram pc)
         (intcode ram (+ pc 4))]
        [else
         (displayln (format "Unknown opcode: ~a" (vector-ref ram pc)))]))))

(define (p1202 in n1 n2)
  (vector-set! in 1 n1)
  (vector-set! in 2 n2)
  (intcode in 0)
  (vector-ref in 0))

; Unit test
(define test1 "1,9,10,3,2,3,11,0,99,30,40,50")
(p1202 (input->vector test1) 9 10)

; Part 1
(p1202 (input->vector input) 12 2)

; Part 2
(define found #f)
(for* ([noun (range 0 99)]
       [verb (range 0 99)]
       #:break found)
  (when (= 19690720 (p1202 (input->vector input) noun verb))
    (displayln (format "Found ~a ~a ~a" noun verb (+ (* 100 noun) verb)))
    (set! found #t)))
  
