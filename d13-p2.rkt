#lang racket

(require "intcode.rkt")
(require "input.rkt")

(define input (first (input-load-lines 13)))

(define (part2 input)
  (define ic (new Intcode%))
  (send ic set-pause-on-output #t)
  (send ic set-debug #t)

  (define (read-i)
    (send ic run)
    (send ic get-output))

  (define h 50)
  (define v 50)
  (define nblks 0)
  (define nblkr 0) ; remain
  (define stage (make-vector 1000 0))
  (define ball-x 0)
  (define ball-y 0)
  (define paddle-x 0)
  (define paddle-y 0)
  (define round 0)
  (define max-r 100000)
  (define gaming #f)
  (define score 0)

  ; Insert coin
  (string-set! input 0 #\2)
  (send ic load-code input)

  (define (paddle-move)
    (let ([m (cond
               [(= ball-x paddle-x) 0]
               [(> ball-x paddle-x) 1]
               [(< ball-x paddle-x) -1])])
      (displayln (format "Pad move: ~a" m))
      (send ic set-input m)))

  (define (print-cabinet)
    ; Print cabinet
    (for ([i (range 0 1000)])
      (when (= (modulo i 50) 0) (displayln ""))
      (let ([c (vector-ref stage i)])
        (when (< (modulo i 50) 46)
          (display (format "~a"
                           (cond
                             [(= c 0) "."]
                             [(= c 1) "#"]
                             [(= c 2) "*"]
                             [(= c 3) "="]
                             [(= c 4) "@"]))))))
    (displayln ""))

  (let loop ()
    ; Make sure use let* to eval one by one
    (define x (read-i))
    (define y (read-i))
    (define c (read-i))

    ; Ouput from Intcode
    (when (not (send ic is-iowait?))
      (when (>= x 0)
        (let ([idx (+ x (* h y))])
          (when (< idx 1000)
            (when (= c 0)
              (when gaming ; Block destroied
                (when (= (vector-ref stage idx) 2)
                  (set! nblks (sub1 nblks)))))
            ; New tile
            (vector-set! stage idx c)))
        (when (= c 2) (set! nblks (add1 nblks)))
        (when (= c 3) (set! paddle-x x) (set! paddle-y y))
        (when (= c 4) (set! ball-x x) (set! ball-y y)))

      ; Score output
      (when (< x 0)
        (set! gaming #t)
        (set! score c)))

    (when (and gaming
               (or (send ic is-iowait?)
                   (send ic is-halt?)))
      (displayln
       (format "Score: ~a, nblocks: ~a, ball: ~a, ~a pad: ~a, ~a r: ~a"
               score nblks ball-x ball-y paddle-x paddle-y round))
      (print-cabinet)

      (set! round (add1 round))
      ;(sleep 0.5)
      (paddle-move))

    (when (not (send ic is-halt?))
      (loop)))
  )

(part2 input)
