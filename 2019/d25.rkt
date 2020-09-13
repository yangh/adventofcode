#lang racket

(require "utils.rkt")
(require "intcode.rkt")
(require "algo-graph.rkt")

(define input (first (input-load-lines 25)))

(define ic (new Intcode%))
(send ic set-pause-on-output #t)
(send ic set-debug #f)
(send ic load-code input)

; Intcode run
(define (intcode-run)
  (send ic run)
  (send ic get-output))

(define new-line 10)

; Vacuum robot
(define va-robot (droid 0 0 NORTH))

(define (update-vacuum-robot x y dir)
  (stage-set! x y DROD)
  (set-droid-x! va-robot x)
  (set-droid-y! va-robot y)
  (set-droid-dir! va-robot dir))

(define (run-to-end)
  (let loop ()
    (let ([ret (intcode-run)])
      (cond
        [(= ret new-line) (displayln "")]
        [(< ret 255) (display (integer->char ret))]
        [else (displayln ret)]))
    (when (not (or (send ic is-halt?)
                   (send ic is-iowait?)))
      (loop))))

(run-to-end)

(define movements (list "NULL" "north" "south" "west" "east"))

(define (mv m)
  (for-each (λ (ascii)
              (send ic set-input ascii))
            (map char->integer (string->list (list-ref movements m))))
  (send ic set-input new-line)
  (run-to-end))

(mv 1)


