#lang racket

(require "input.rkt")

(define input (first (input-load-lines 2)))

(define (input->vector input) (list->vector
                               (map string->number
                                    (string-split input ","))))

;(displayln (format "Code length: ~a" (vector-length ins)))

(define (value-at ints pos)
  (vector-ref ints (vector-ref ints pos)))

(define (op3 op ints pos)
  (vector-set! ints
               (vector-ref ints (+ pos 3))
               (op (value-at ints (+ pos 1))
                   (value-at ints (+ pos 2)))))

(define (intcode ints pos)
  (when (< pos (vector-length ints))
    ;(displayln (format "Pos: ~a" pos))
    (let ([op (vector-ref ints pos)])
      (cond
        [(= op 99) #f]
        [(= op 1)
         (op3 + ints pos)
         (intcode ints (+ pos 4))]
        [(= op 2)
         (op3 * ints pos)
         (intcode ints (+ pos 4))]
        [else
         (displayln (format "Unknown opcode: ~a" (vector-ref ints pos)))]))))

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
  
