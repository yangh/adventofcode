#lang racket

(require "intcode.rkt")
(require "input.rkt")

(define input (first (input-load-lines 11)))

(define turn-l #hasheqv((#\< . #\v)
                        (#\v . #\>)
                        (#\> . #\^)
                        (#\^ . #\<)))
(define turn-r #hasheqv((#\< . #\^)
                        (#\^ . #\>)
                        (#\> . #\v)
                        (#\v . #\<)))
(define move-h #hasheqv((#\< . (-1  0))
                        (#\v . ( 0  1))
                        (#\> . ( 1  0))
                        (#\^ . ( 0 -1))))

(define (part1 input n)
  (define d #\^) ; Current direction
  (define p 100) ; Panel size, estimated
  (define x (/ p 2)) ; Current position
  (define y (/ p 2))
  (define panelv (make-vector (* p p) 0)) ; Record panel color info
  (define panels (make-hasheqv)) ; Record painted panels

  (define (current-idx) (+ x (* p y)))

  (define (turn t)
    (when (not (or (= t 0) (= t 1)))
      (displayln (format "ERROR: unknown turn: ~a" t)))
    (let* ([turn-h (if (= t 0) turn-l turn-r)]
           [dir (hash-ref turn-h d)]
           [mov (hash-ref move-h dir)])
      ;(displayln (format "Move: ~a,~a ~a ~a, ~a" x y mov dir (vector-ref panelv (current-idx))))
      (set! d dir)
      (set! x (+ x (list-ref mov 0)))
      (set! y (+ y (list-ref mov 1)))))

  (define ic (new Intcode%))
  (send ic set-pause-on-output #t)
  ;(send ic set-debug #t)
  (send ic load-code input)

  ; Init color
  (vector-set! panelv (current-idx) n)

  ; Paint loop
  (let loop ([i n])
    (send ic set-input i)
    (let ([idx (current-idx)])
      (send ic run)
      (let ([c (send ic get-output)])
        (vector-set! panelv idx c)
        ;(displayln (format "Panel color ~a -> ~a" i c))
        (hash-set! panels idx c)))

    (send ic run)
    (turn (send ic get-output))
    (when (not (send ic is-halt?))
      (loop (vector-ref panelv (current-idx)))))

  ; Paint Result
  (for ([y (range 0 p)])
    (for ([x (range 0 p)])
      (let ([c (vector-ref panelv (+ x (* p y)))]
            [ped (hash-has-key? panels (+ x (* p y)))])
        (cond
          [(= c 1) (display "#")]
          ;[(and (= c 0) ped) (display "o")]
          [else (display ".")])))
    (displayln ""))

  (displayln (format "painted: ~a" (hash-count panels))))

(part1 input 0)

; Part2
(part1 input 1)