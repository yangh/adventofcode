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

(define wx 0)
(define x 0)
(define y 0)

; Explore the map
(let loop ()
  (let ([n (intcode-run)])
    (cond
      [(= n 10) (displayln "")
                (set! wx (max x wx))
                (set! x 0)
                (set! y (add1 y))]
      [(= n 35) (display "#")
                (path-add x y)]
      [(= n 46) (display ".")]
      [(= n 94) (display "^")]
      [(= n 62) (display ">")]
      [(= n 60) (display "<")]
      [(= n 118) (display "v")]
      [else     (display "*")])
    (set! x (add1 x))
    (stage-set! x y n))
  (when (not (or (send ic is-halt?)
                 (send ic is-iowait?)))
    (loop)))

(set-h! wx)
(set-v! y)

(foldl
 (λ (pos result)
   (let ([px (first pos)]
         [py (second pos)])
     (when (and (> px 0) (< px h)
                (> py 0) (< py v))
       (displayln (format "Check pos: ~a" pos))
       (let ([dirs (find-dir-open-at (first pos) (second pos) (list 35))])
         (when (= 4 (length dirs))
           (displayln (format "Intersection: ~a, ~a" pos dirs))))))
   0)
 0 paths)