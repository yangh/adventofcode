#lang racket

(require "input.rkt")

(define input (first (input-load-lines 8)))

(define min-z 100000)
(define min-l 0)
(define n1 0)
(define n2 0)

(define (count-of c str)
  (foldl (lambda (x r)
           (+ r (if (char=? x c) 1 0)))
         0 (string->list str)))

(let ([ln (string-length input)])
  (for ([i (range (/ ln (* 25 6)))])
    (let* ([s1 (* i (* 25 6))]
           [s2 (* (add1 i) (* 25 6))]
           [str (substring input s1 s2)])
      (let ([nz (count-of #\0 str)])
        (displayln (format "Layer: ~a, n-of-zeror: ~a" i nz))
        (when (< nz min-z)
          (set! n1 (count-of #\1 str))
          (set! n2 (count-of #\2 str))
          (set! min-z nz)
          (set! min-l i))))))

min-z
min-l
n1
n2
; Part 1
(* n1 n2)

; Merge layer by pixel
(define (determine str i)
  (let ([ln (string-length input)])
    (let loop ([li 0])
      (let* ([idx (+ (* li (* 25 6)) i)]
             [c (string-ref str idx)])
        (cond
          [(= li 99) c]
          [(char=? #\2 c) (loop (add1 li))]
          [else c])))))

; Merge layers
(define output (make-string (* 25 6) #\0))
(for ([i (range 0 (* 25 6))])
  (string-set! output i (determine input i)))

; Part 2, decoded code
(for ([i (range 0 6)])
  (displayln (substring output (* i 25) (* (add1 i) 25))))

