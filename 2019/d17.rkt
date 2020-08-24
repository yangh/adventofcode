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
      [(= n 10) (displayln "")] ; New line
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

(define (path-to-turn-letter robot m-delta)
  (let* ([dir-idx (list-ref (move-delta-of-dir (droid-dir robot)) 3)]
         [m-idx (list-ref m-delta 3)])
    ;(displayln (format "Droid ~a, dir-idx: ~a, move: ~a" robot dir-idx m-delta))
    (cond
      [(= m-idx dir-idx) #\F] ; Forward
      [(= m-idx (modulo (add1 dir-idx) 4)) #\R]
      [(= m-idx (modulo (sub1 dir-idx) 4)) #\L]
      [else #\X])))

(define (robot-move robot m-delta)
  (let ([nx (+ (droid-x robot) (list-ref m-delta 0))]
        [ny (+ (droid-y robot) (list-ref m-delta 1))]
        [dir (list-ref m-delta 2)])
    (set-droid-x! robot nx)
    (set-droid-y! robot ny)
    (set-droid-dir! robot dir)
    (set-current-droid-pos! nx ny)
    (stage-set! nx ny ROAD)
    (list nx ny)))

(define (find-direct-path robot)
  (let ([dirs (find-dir-open-at (droid-x robot) (droid-y robot) (list WALL))])
    (cond
      [(= 0 (length dirs))
       (ddisplayln (format "End of path"))
       (let ([m-delta (move-delta-of-dir (droid-dir robot))])
         (robot-move robot m-delta)
         (path-to-turn-letter robot m-delta))]
      [(= 1 (length dirs))
       (let* ([m-delta (second (first dirs))]
              [turn (path-to-turn-letter robot m-delta)])
         (ddisplayln (format "Turn: ~a" turn))
         (path-remove (robot-move robot m-delta))
         turn)]
      [else
       ; TODO: Find forward path first
       (ddisplayln (format "Multiple paths found: ~a" dirs))
       (let ([f-dirs (filter (λ (m) (= (droid-dir robot) (list-ref (second m) 2))) dirs)])
         (cond
           [(= 1 (length f-dirs))
            (let ([m-delta (second (first f-dirs))])
              (ddisplayln (format "Forward: ~a" m-delta))
              (path-remove (robot-move robot m-delta))
              (path-to-turn-letter robot m-delta))]
           [else
            (displayln (format "Unkown how to go: ~a" dirs))
            #f]))])))

;(find-direct-path va-robot)

(define steps '())

(let loop ()
  (when (> (path-len) 0)
    ;(dump-stage 0.05 #t)
    (set! steps (append steps (list (find-direct-path va-robot))))
    (loop)))

;(dump-stage 0.05 #t)

;(displayln steps)

(define cnt 0)
(for-each (λ (s)
            (cond
              [(or (char=? s #\R) (char=? s #\L))
               (cond
                 [(= cnt 0) (set! cnt 1)
                            (path-push s)]
                 [else (path-push cnt)
                       (path-push s)
                       (set! cnt 1)])]
              [else (set! cnt (add1 cnt))]))
          steps)
(path-push cnt)

; Full steps to wall through all the paths
(displayln (reverse paths))

(define (steps-to-ascii steps)
  (map (λ (s)
         (cond
           [(char? s) (char->integer s)]
           [else s]))
       (string->list (string-replace steps " " ","))))

;(steps-to-ascii (reverse paths))
;;;
; TODO: How find out 3 paths to cover all the steps by algorithm?
;
;(R 12 L 8 L 4 L 4      < A
;   L 8 R 6 L 6         < B
; R 12 L 8 L 4 L 4
;   L 8 R 6 L 6
; L 8 L 4 R 12 L 6 L 4  < C
; R 12 L 8 L 4 L 4
; L 8 L 4 R 12 L 6 L 4
; R 12 L 8 L 4 L 4
; L 8 L 4 R 12 L 6 L 4
;   L 8 R 6 L 6)
;
; Main:
;   A B A B C A C A C B
;;;

(define A "R 12 L 8 L 4 L 4")
(define B "L 8 R 6 L 6")
(define C "L 8 L 4 R 12 L 6 L 4")
(define MAIN "A B A B C A C A C B")
(define VIDEO "n")

;(steps-to-ascii A)
(define (part2)
  (send ic load-code (first (input-load-lines 17)))
  (send ic set-debug #f)
  ; Reinit for Part2
  (send ic value-set! 0 2)
  (send ic set-pause-on-output #t)

  (define (run-to-end)
    (let loop ()
      (let ([ret (intcode-run)])
        (cond
          [(= 10 ret)  (displayln "")]
          [(< ret 255) (display (integer->char ret))]
          [else (displayln ret)]))
      (when (not (send ic is-halt?)) (loop))))
  
  (define (load-program prog)
    (for-each (λ (c) (send ic set-input c))
              (steps-to-ascii prog))
    ; progame end by newline '10
    (send ic set-input 10))

  (load-program MAIN)
  (load-program A)
  (load-program B)
  (load-program C)
  (load-program VIDEO)
  (send ic dump)
  (run-to-end)
  )

; Output 761085
(part2)