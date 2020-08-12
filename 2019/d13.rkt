#lang racket

(require "intcode.rkt")
(require "input.rkt")

(define input (first (input-load-lines 13)))

(define (part1 input)
  (define ic (new Intcode%))
  (send ic set-pause-on-output #t)
  ;(send ic set-debug #t)

  (define (read-i)
    (send ic run)
    (send ic get-output))

  (define h 50)
  (define v 50)
  (define nblks 0)
  (define stage (make-vector 1000 0))

  (send ic load-code input)
  (let loop ()
    ; Make sure use let* to eval one by one
    (define x (read-i))
    (define y (read-i))
    (define c (read-i))
    (vector-set! stage (+ x (* h y)) c)
    (when (= c 2) (set! nblks (add1 nblks)))
    ;(displayln (format "[~a][~a] ~a" x y c))
    (when (not (send ic is-halt?))
      (loop)))

  ; Part 1
  (displayln (format "Blocks: ~a, ~a, 2 = ~a" h v nblks))
  
  (for ([i (range 0 1000)])
    (when (= (modulo i 50) 0) (displayln ""))
    (let ([c (vector-ref stage i)])
      (when (< (modulo i 50) 46)
        (display (format "~a" (if (= c 0) "." c))))))
  (displayln "")
  )

(part1 input)

