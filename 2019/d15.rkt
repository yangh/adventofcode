#lang racket

(require "intcode.rkt")
(require "utils.rkt")
(require "algo-graph.rkt")

(define input (first (input-load-lines 15)))

(define ic (new Intcode%))
(send ic set-pause-on-output #t)
;(send ic set-debug #t)

; Init position
(define x (/ h 2))
(define y (/ v 2))

(define (reset-xy)
  (set! x (/ h 2))
  (set! y (/ v 2)))

; Oxygen System position
(define ox 0)
(define oy 0)
     
; Update current position
; and reduce the weight of the passed position for DFS?/BFS?
(define (update-pos dir obj update-axis)
  (let* ([axis-off dir]
         [nx (+ x (first axis-off))]
         [ny (+ y (second axis-off))]
         [idx (+ nx (* ny h))])
    (stage-set! nx ny obj)
    (when update-axis
      (when (not (= obj WALL))
        (set! x nx)
        (set! y ny)
        (path-add x y))
      (stage-weight-sub1 nx ny))))

; Intcode move
(define (intcode-move dir)
  (send ic set-input (third dir))
  (send ic run)
  (send ic get-output))

; Move
(define (move dirs)
  (let ([dir (find-weightest-dir dirs)])
    (let ([ret (intcode-move dir)])
      (ddisplayln (format "Move ~a, ret ~a" dir ret))
      (cond
        [(= ret WALL)
         ;(displayln "It's wall")
         (update-pos dir ret #f)]
        [(= ret ROAD)
         (retrofit dir dirs)
         (update-pos dir ret #t)
         (ddisplayln (format "Moved ~a ~a" x y))]
        [(= ret OXYG)
         (update-pos dir ret #t)
         ;(dump-stage)
         (ddisplayln (format "Moved ~a ~a" x y))
         (displayln (format "Found Oxygen: ~a ~a" x y))
         (set! ox x)
         (set! oy y)])
      ret)))

; Explore more blocks before move forward
(define (retrofit dir dirs)
  ; Move back
  (intcode-move (get-move-back-dir dir))

  ; Try each dirs
  (for-each
   (lambda (d)
     (when (not (= (list-ref dir 3)
                   (list-ref d 3)))
       (let ([ret (intcode-move d)])
         (ddisplayln (format "Retrofit Move ~a, ret ~a" d ret))
         (cond
           [(= ret WALL)
            ;(displayln "It's wall")
            (update-pos d ret #f)]
           [(= ret ROAD)
            (update-pos d ret #f)
            ; Move back
            (intcode-move (get-move-back-dir d))]
           [(= ret OXYG)
            (update-pos d ret #f)
            ; Move back
            (intcode-move (get-move-back-dir d))
            ;TODO Do some thing
            (displayln (format "Found Oxygen - Retrofit: ~a ~a" x y))
            (set! ox x)
            (set! oy y)])
         ;(dump-stage)
         )))
   (map second dirs))

  ; Move forward
  (intcode-move dir))

(define (go)
  (send ic load-code input)
  (let loop ()
    ;(displayln "Loop")
    (let ([dirs (find-dir-open-at x y (list EMPT ROAD OXYG))])
      (cond
        [(= 0 (length dirs)) (displayln "Dead end")]
        [else
         (ddisplayln (format "Dirs x ~a y ~a, ~a" x y dirs))
         ;(when (= 1 (length dirs)) (displayln "Dead end found, fall back"))
         (let ([ret (move dirs)])
           (when (not (= OXYG ret))
             ;(dump-stage 0.04)
             (loop)))]))))

; Part 1
(define (part1)
  (set-move-clock-wise)
  (go)
  (reset-xy) ; Move D to original position
  (dump-stage)
  ;(dump-stage-weight2)
  (path-dump-len))

(part1)

; Part 2
; Fill all the area with oxygen gradually
(define open-dirs (list (list ox oy)))

(define (pos-move-to x y dir)
  (list (+ x (first dir)) (+ y (second dir))))

(define (fill-oxygen)
  (define new-dirs '())
  (for-each (λ (o)
              (let* ([x (first o)]
                     [y (second o)]
                     [idx (+ x (* y h))]
                     [dirs (find-dir-open-at x y (list ROAD))])
                ;(displayln (format "Open dirs at ~a ~a: ~a" x y dirs))
                (vector-set! stage idx OXYG)
                (for-each (λ (d) (set! new-dirs (append new-dirs (list (pos-move-to x y (second d))))))
                          dirs)))
            open-dirs)
  (set! open-dirs new-dirs))

(define (go-fill)
  (let loop ()
    (cond
      [(empty? open-dirs)
       (displayln (format "Full filled in ~a steps" (path-len)))]
      [else
       (fill-oxygen)
       (when (not (empty? open-dirs))
         (path-push (list (length open-dirs) open-dirs)))
       (when (= -1 (modulo (path-len) 1))
         (dump-stage 0.2))
       (loop)])))

;Try to find all road & wall
; Round 2 to explore full map
; WARN: Assume the max branch number is 2.
(define (part2)
  ; Depends on part 1
  (path-reset)
  (go)
  (reset-xy)
  (dump-stage)
  (path-dump-len)

  (path-reset)
  (go-fill))

(part2)
