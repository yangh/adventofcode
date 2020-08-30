#lang racket

(require "intcode.rkt")
(require "utils.rkt")
(require "algo-graph.rkt")

(define input (first (input-load-lines 19)))

(define ic (new Intcode%))
(send ic set-pause-on-output #t)
(send ic set-debug #f)

; Intcode run
;  return value:
;    1 point is in the tractor beam
;    0 point is out of the tractor beam
(define (intcode-run x y)
  ; The program must be reload for each runtine
  ; It takes many time for each point, thus a
  ; good algorithm is needed.
  (send ic load-code input)
  (send ic set-input x)
  (send ic set-input y)
  (send ic run)
  (send ic get-output))

(define x-offset 0)
(define y-offset 0)

; Previsour line start x
(define pre-line-start-x 0)
(define pre-line-end-x 0)

(define (part1 space-h space-w)
  (stage-init space-w space-h ROAD)
  (set! pre-line-end-x space-w)

  (for ([by (range 0 space-h)])
    (define line-start #f)
    (define line-end #f)

    ; Find line start
    (for ([bx (range pre-line-start-x space-w)]
          #:break (and line-start))
      (let* ([x (+ bx x-offset)]
             [y (+ by y-offset)]
             [ret (intcode-run x y)])
        (when (and (= 1 ret)
                   (not line-start))
          (set! line-start #t)
          (set! pre-line-start-x bx))))

    ; Find line end
    (for ([bx (range (add1 pre-line-end-x) (sub1 pre-line-start-x) -1)]
          #:break (and line-end))
      (let* ([x (+ bx x-offset)]
             [y (+ by y-offset)]
             [ret (intcode-run x y)])
        (when (and (= 1 ret)
                   (not line-end))
          (set! line-end #t)
          (set! pre-line-end-x (add1 bx)))
        ; No point found
        (when (and (= 0 ret)
                   (= bx pre-line-start-x))
          (set! line-end #t)
          (set! pre-line-end-x bx))))
    
    (ddisplayln (format "Start, end: ~a ~a" pre-line-start-x pre-line-end-x))

    ; Fill the line
    (for ([bx (range pre-line-start-x pre-line-end-x)])
      (stage-set! bx by WALL)
      (let ([x (+ bx x-offset)]
            [y (+ by y-offset)])
        (when (not (path-has (list x y)))
          (path-add x y)))))

  (dump-stage)
  (displayln (format "Points effected ~a/~a square: ~a"
                     space-w space-h (path-len))))

; Part 1, 156
(part1 50 50)

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

; Only need to check 3 corners of a square
;  (x, y) (x+n, y) (x, y+n)
;
;   0--0
;   |
;   0

(define (is-square-n x y n)
  (and (= 1 (intcode-run x y))
       (= 1 (intcode-run (+ x (sub1 n)) y))
       (= 1 (intcode-run x (+ y (sub1 n))))))

; We scan in backward, we start from 300,1100 because
; each square found in the 50x50 range has a similar
; distance: 3, 10

; Square 100 position guess
(define x100 300)
(define y100 1000)
(define sq100 100)

(define (part2)
  (set! pre-line-start-x 1)
  (set! pre-line-end-x (+ x100 sq100))

  (for ([by (range y100 (- y100 sq100) -1)])
    (define line-start #f)
    (define line-end #f)

    ; Find line start
    (for ([bx (range (sub1 pre-line-start-x) x100)]
          #:break (and line-start))
      (let* ([x (+ bx x-offset)]
             [y (+ by y-offset)]
             [ret (intcode-run x y)])
        (when (= 1 ret)
          (set! line-start #t)
          (set! pre-line-start-x bx))))

    ; Find line end
    (for ([bx (range (add1 pre-line-end-x) (sub1 pre-line-start-x) -1)]
          #:break (and line-end))
      (let* ([x (+ bx x-offset)]
             [y (+ by y-offset)]
             [ret (intcode-run x y)])
        (when (= 1 ret)
          (set! line-end #t)
          (set! pre-line-end-x bx))))

    ;(displayln (format "Pre x start/end: ~a ~a" pre-line-start-x pre-line-end-x))
    
    (define found #t)

    ; Find all square on the line
    (for ([bx (range (add1 (- pre-line-end-x sq100)) pre-line-start-x -1)]
          #:break (not found))
      (let ([x (+ bx x-offset)]
            [y (+ by y-offset)])
        (if (is-square-n x y sq100)
            (path-add x y)
            (set! found #f))))
    ))

; Part 2, Square 100 at: 261, 980
(part2)

; Multiple sqaure return, take the first which is nearest
(let* ([pos (first paths)]
       [x (first pos)]
       [y (second pos)])
  (displayln (format "Nearest Square 100 at: ~a, ~a" x y))
  (displayln (format "Part 2 answer: ~a" (+ (* 10000 x) y))))
