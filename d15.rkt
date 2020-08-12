#lang racket

(require "intcode.rkt")
(require "input.rkt")

(define input (first (input-load-lines 15)))

(define dbg #f)

(define (ddisplayln msg)
  (when #t; (> ox 0)
    (displayln msg)))

; Direction
(define mn 1) ; North
(define ms 2) ; South
(define mw 3) ; West
(define me 4) ; East

; Stage blocks index
(define WALL 0)
(define ROAD 1)
(define OXYG 2)
(define DROD 3)
(define EMPT 4)
(define stage-strs (list  "#" "." "@" "D" " "))

(define move-delta
  ; Move  x,y,direction,index
  (list '(0 -1 1 0) ; North
        '(1  0 4 1) ; East
        '(0  1 2 2) ; South
        '(-1 0 3 3) ; West
        ))

; Choose move back direction
(define (get-move-back-dir dir)
  (let ([idx (list-ref dir 3)])
    (list-ref move-delta (modulo (+ idx 2) 4))))

(define ic (new Intcode%))
(send ic set-pause-on-output #t)
;(send ic set-debug #t)

(define h 60)
(define v h)

; Init position
(define x (/ h 2))
(define y (/ v 2))

(define (reset-xy)
  (set! x (/ h 2))
  (set! y (/ v 2)))

; Oxygen System position
(define ox 0)
(define oy 0)

; Stage
(define stage (make-vector (* h v) EMPT))

; How many times we stepped on this point
(define stage-weight2 (make-vector (* h v) 0))

(define (update-pos dir ret update-axis)
  (let* ([axis-off dir]
         [nx (+ x (first axis-off))]
         [ny (+ y (second axis-off))]
         [idx (+ nx (* ny h))])
    (when (and update-axis (not (= ret WALL)))
      (set! x nx)
      (set! y ny))
    (vector-set! stage idx ret)
    (when update-axis
      (vector-set! stage-weight2 idx
                   (sub1 (vector-ref stage-weight2 idx))))))

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
         (displayln (format "Found Oxygen: ~a ~a" x y))
         (set! ox x)
         (set! oy y)])
      ret)))

(define (find-dir-open)
  (foldl
   (lambda (dir result)
     (append result
             (let* ([nx (+ x (first dir))]
                    [ny (+ y (second dir))]
                    [idx (+ nx (* ny h))]
                    [c (vector-ref stage idx)]
                    [w2 (vector-ref stage-weight2 idx)])
               ;(ddisplayln (format "Find ~a ~a ~a ~a" nx ny c w2))
               (cond
                 [(= c EMPT) (list (list (+ c w2) dir))]
                 [(= c ROAD) (list (list (+ c w2) dir))]
                 [else '()]))))
   '() move-delta))

(define (dump-stage)
  ; Print cabinet
  (for ([i (range 0 h)]) (display "-"))
  (displayln "")

  (let ([didx (+ x (* y h))])
    (for ([i (range 0 (* h v))])
      (when (= (modulo i h) (sub1 h)) (displayln ""))
      (let ([c (vector-ref stage i)])
        (if (= i didx)
            (display "D")
            (display (list-ref stage-strs c))))))
  (displayln ""))

; Select way by weight
(define (find-weightest-dir dirs)
  (define dir (first dirs))
  (for-each
   (lambda (d)
     (when (> (first d) (first dir))
       (set! dir d)))
   (cdr dirs))
  (second dir))

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
         (dump-stage))))
   (map second dirs))

  ; Move forward
  (intcode-move dir)
  )

(define (go)
  (send ic load-code input)
  (let loop ()
    (displayln "Loop")
    (let ([dirs (find-dir-open)])
      (cond
        [(= 0 (length dirs)) (displayln "Dead end")]
        [else
         (ddisplayln (format "Dirs x ~a y ~a, ~a" x y dirs))
         (let ([ret (move dirs)])
           (when (not (= OXYG ret))
             (dump-stage)
             (sleep 0.08)
             (loop)))]))))

(go)
(reset-xy)
(dump-stage)

;TODO: Wall on the road, and try to find all road & wall

;(part1 input)