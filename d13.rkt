#lang racket

(require "intcode.rkt")
(require "input.rkt")

(define input (first (input-load-lines 13)))

(define (part1 input)
  (define ic (new Intcode%))
  (send ic set-pause-on-output #t)
  ;(send ic set-debug #t)
  (send ic load-code input)

  (define (read-i)
    (send ic run)
    (send ic get-output))

  (define h 0)
  (define v 0)
  (define nblks 0)
  (define stage (make-vector 1600 0))

  (let loop ()
    ; Make sure use let* to eval one by one
    (define x (read-i))
    (define y (read-i))
    (define c (read-i))
    (set! h (max h x))
    (set! v (max v y))
    (vector-set! stage (+ x (* x y)) c)
    (when (= c 2) (set! nblks (add1 nblks)))
    ;(displayln (format "[~a][~a] ~a" y x c))
    (when (not (send ic is-halt?))
      (loop)))

  ; Part 1
  (displayln (format "Blocks: ~a, ~a, 2 = ~a" h v nblks))
  
  (for* ([y (range 0 (add1 v))]
         [x (range 0 (add1 h))])
    (let ([c (vector-ref stage (+ x (* x y)))])
      (display (format "~a" (if (= c 0) " " c))))
    (when (= x h) (displayln "")))
  )

(part1 input)