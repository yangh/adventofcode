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

; Put the map in the middle of the stage
(define xy-init 2)
(define x xy-init)
(define y xy-init)

; Vacuum robot
(define va-robot (droid 0 0 NORTH))

(define (update-vacuum-robot x y dir)
  (stage-set! x y DROD)
  (set-droid-x! va-robot x)
  (set-droid-y! va-robot y)
  (set-droid-dir! va-robot dir))

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
      [(= n 94) (display "^")
                (update-vacuum-robot x y NORTH)]
      [(= n 62) (display ">")
                (update-vacuum-robot x y EAST)]
      [(= n 60) (display "<")
                (update-vacuum-robot x y WEST)]
      [(= n 118) (display "v")
                 (update-vacuum-robot x y SOUTH)]
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

(define (alignment-params pos)
  (let* ([px (first pos)]
         [py (second pos)]
         [orig-x (- px xy-init)]
         [orig-y (- py xy-init)]
         [dirs (find-dir-open-at px py (list WALL))])
    ;(displayln (format "Check pos: ~a" pos))
    (cond
      [(= 4 (length dirs))
       (ddisplayln (format "Intersection: ~a, ~a" orig-x orig-y))
       ;(stage-set! px py OXYG)
       (* orig-x orig-y)]
      [else 0])))

; Part 1: sum of the alignment parameters
(foldl + 0 (map alignment-params paths))

;(dump-stage)


