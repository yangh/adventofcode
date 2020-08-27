#lang racket

(require "intcode.rkt")
(require "utils.rkt")
(require "algo-graph.rkt")

(define input (first (input-load-lines 19)))

(define ic (new Intcode%))
(send ic set-pause-on-output #t)
(send ic set-debug #f)

; Intcode run
(define (intcode-run x y)
  ; The program must be reload for each runtine
  ; It takes more time for bigger x/y, thus a
  ; good algorithm is needed.
  (send ic load-code input)
  (send ic set-input x)
  (send ic set-input y)
  (send ic run)
  (send ic get-output))

(define x-offset 0)
(define y-offset 0)

(define (part1 space)
  (stage-set-all! ROAD)
  (for ([by (range 0 space)])
    (define line-start #f)
    (define line-end #f)
    (for ([bx (range 0 space)] #:break (and line-start line-end))
      (let* ([x (+ bx x-offset)]
             [y (+ by y-offset)]
             [ret (intcode-run x y)])
        (cond
          [(= 1 ret)
           (when (not line-start) (set! line-start #t))
           (stage-set! bx by WALL)
           (path-add x y)]
          [else
           (when line-start (set! line-end #t))]))))

  (dump-stage)
  (displayln (format "Points effected: ~a" (path-len))))

; Part 1, 156
(part1 50)

; It seem that null pos takes longer time to detect
(define (null-tl)
  (for-each
   (λ (pos)
     (intcode-run (first pos) (second pos)))
   paths))

(define (stars-on-dir pos dir)
  (let ([m-delta (move-delta-of-dir dir)])
    (let loop ([n 1]
               [x (first pos)]
               [y (second pos)])
      (let* ([nx (+ x (first m-delta))]
             [ny (+ y (second m-delta))])
        (if (stage-obj-is nx ny WALL)
            (loop (add1 n) nx ny)
            n)))))

; Find all squares in the tractor beam
(define (find-squares)
  (for-each
   (λ (pos)
     (let ([width (stars-on-dir pos EAST)]
           [height (stars-on-dir pos SOUTH)])
       (when (= width height)
         (displayln (format "~a: ~a" width pos)))))
   (reverse paths)))

;(find-squares)

(set! x-offset 260)
(set! y-offset 1100)
;(part1 40)

; Square 100 position
(define x100 0)
(define y100 0)

; We scan in backward, we start from 300,1100 because
; each square found in the 50x50 range has a similar
; distance: 3, 11
;
; Only need to check 3 corners of a square
;  (x, y) (x+n, y) (x, y+n) 

(define (is-square-n x y n)
  (and (= 1 (intcode-run x y))
       (= 1 (intcode-run (+ x (sub1 n)) y))
       (= 1 (intcode-run x (+ y (sub1 n))))))

(let loopx ([x 300]
            [y 1100])
  (cond
    [(is-square-n x y 100)
     (loopx (sub1 x) y)]
    [else
     (set! x100 (add1 x))
     (displayln (format "~a, ~a is the last" x y))]))

(let loopy ([x x100]
            [y 1100])
  (cond
    [(is-square-n x y 100)
     (loopy x (sub1 y))]
    [else
     (set! y100 (add1 y))
     (displayln (format "~a, ~a is the last" x y))]))

; Part 2, Square 100 at: 290, 1058
(displayln (format "Square 100 at: ~a, ~a" x100 y100))
(displayln (format "Part 2 answer: ~a" (+ (* 10000 x100) y100)))
