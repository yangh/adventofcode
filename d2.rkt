#lang racket

(define input "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,5,23,1,23,6,27,2,9,27,31,1,5,31,35,1,35,10,39,1,39,10,43,2,43,9,47,1,6,47,51,2,51,6,55,1,5,55,59,2,59,10,63,1,9,63,67,1,9,67,71,2,71,6,75,1,5,75,79,1,5,79,83,1,9,83,87,2,87,10,91,2,10,91,95,1,95,9,99,2,99,9,103,2,10,103,107,2,9,107,111,1,111,5,115,1,115,2,119,1,119,6,0,99,2,0,14,0")

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
        [(= op 99)
         ;(displayln "99")
         ;(displayln ints)
         #f
         ]
        [(= op 1)
         (op3 + ints pos)
         (intcode ints (+ pos 4))]
        [(= op 2)
         (op3 * ints pos)
         (intcode ints (+ pos 4))]
        [else
         (displayln (format "Unknown opcode: ~a" (vector-ref ints pos)))]))))

(define test1 "1,9,10,3,2,3,11,0,99,30,40,50")
(intcode (input->vector test1) 0)

(define (p1202 in n1 n2)
  (vector-set! in 1 n1)
  (vector-set! in 2 n2)
  (intcode in 0)
  (vector-ref in 0))

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
  
