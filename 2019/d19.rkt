#lang racket

(require "intcode.rkt")
(require "utils.rkt")
(require "algo-graph.rkt")

(define input (first (input-load-lines 19)))

(define ic (new Intcode%))
(send ic set-pause-on-output #t)
(send ic set-debug #f)

; Intcode run
(define (intcode-run)
  (send ic run)
  (send ic get-output))

(for* ([x (range 0 50)]
       [y (range 0 50)])
  (send ic load-code input)
  (send ic set-input x)
  (send ic set-input y)
  (let ([ret (intcode-run)])
    (cond
      [(= 1 ret)
      (stage-set! x y WALL)
      (path-add x y)]
      [else
       (stage-set! x y ROAD)
       ])))

(dump-stage)
(path-len)