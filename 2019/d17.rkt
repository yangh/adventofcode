#lang racket

(require "intcode.rkt")
(require "utils.rkt")
(require "algo-graph.rkt")

(define input (first (input-load-lines 17)))

(define ic (new Intcode%))
(send ic set-pause-on-output #t)
(send ic set-debug #f)

; Intcode run
(define (intcode-run)
  (send ic run)
  (send ic get-output))

(send ic load-code input)

(define xy-init 2)
(define x xy-init)
(define y xy-init)

; Explore the map
(let loop ()
  (let ([n (intcode-run)])
    ; Print the map
    (cond
      [(= n 10) (displayln "")]
      [(= n 35) (display "#")
                (stage-set! x y WALL)
                (path-add x y)]
      [(= n 46) (display ".")
                (stage-set! x y ROAD)]
      [(= n 94) (display "^")]
      [(= n 62) (display ">")]
      [(= n 60) (display "<")]
      [(= n 118) (display "v")]
      [else     (display "*")])
    ; Update x, y
    (cond
      [(= n 10)
       (set! x xy-init)
       (set! y (add1 y))]
      [else
       (set! x (add1 x))]))

  (when (not (or (send ic is-halt?)
                 (send ic is-iowait?)))
    (loop)))

; Part 1: sum of the alignment parameters
(foldl + 0
       (map (λ (pos)
              (let ([px (first pos)]
                    [py (second pos)])
                ;(displayln (format "Check pos: ~a" pos))
                (let ([dirs (find-dir-open-at px py (list WALL))])
                  (cond
                    [(= 4 (length dirs))
                     (ddisplayln (format "Intersection: ~a, ~a" (- px xy-init) (- py xy-init)))
                     (stage-set! px py OXYG)
                     (* (- px xy-init) (- py xy-init))]
                    [else 0]))))
            paths))

;(dump-stage)