#lang racket

(require "intcode.rkt")
(require "input.rkt")

(define input (first (input-load-lines 15)))

(define dbg #f)

(define (ddisplayln msg)
  (when #t; (> ox 0)
    (displayln msg)))

(define mn 1) ; North
(define ms 2) ; South
(define mw 3) ; West
(define me 4) ; East

(define WALL 0)
(define ROAD 1)
(define OXYG 2)
(define EMPT 4)

(define m-delta
  (list '(0 -1 1) ; North
        '(1  0 4) ; East
        '(0  1 2) ; South
        '(-1 0 3) ; West
        ))

(define ic (new Intcode%))
(send ic set-pause-on-output #t)
;(send ic set-debug #t)

(define h 50)
(define v h)

(define stage (make-vector (* h v) 4))

; How many times we stepped on this point
(define stage-weight2 (make-vector (* h v) 0))

(define (update-pos dir ret)
  (let* ([axis-off dir]
         [nx (+ x (first axis-off))]
         [ny (+ y (second axis-off))]
         [idx (+ nx (* ny h))])
    (when (not (= ret 0))
      (set! x nx)
      (set! y ny))
    (vector-set! stage idx ret)
    (vector-set! stage-weight2 idx
                 (sub1 (vector-ref stage-weight2 idx)))))

; Move
(define (move dir)
  (send ic set-input (third dir))
  (send ic run)
  (let ([ret (send ic get-output)])
    (update-pos dir ret)
    (cond
      ;[(= ret WALL) (displayln "It's wall")]
      [(= ret ROAD) (ddisplayln "Moved")]
      [(= ret OXYG)
       (displayln (format "Found Oxygen: ~a ~a" x y))
       (set! ox x)
       (set! oy y)
       (dump-stage)])
    ret))

; Init position
(define x (/ h 2))
(define y (/ v 2))

(define (reset-xy)
  (set! x (/ h 2))
  (set! y (/ v 2)))

(define ox 0)
(define oy 0)

(send ic load-code input)

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
   '() m-delta))

(define (dump-stage)
  ; Print cabinet
  (for ([i (range 0 h)]) (display "-"))
  (displayln "")

  (let ([didx (+ x (* y h))])
    (for ([i (range 0 (* h v))])
      (when (= (modulo i h) (sub1 h)) (displayln ""))
      (let* ([c (vector-ref stage i)]
             [di (+ (/ h 2) (* (/ v 2) h))])
        (if (= i didx)
            (display "D")
            (display (format "~a"
                             (cond
                               [(= c 0) "#"]
                               [(= c 1) "."]
                               [(= c 2) "@"]
                               [(= c 3) "D"]
                               [(= c 4) " "])))))))
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

(define (go)
  (let loop ()
    (let ([dirs (find-dir-open)])
      (cond
        [(= 0 (length dirs)) (displayln "Dead end")]
        [else
         (ddisplayln (format "Dirs x ~a y ~a, ~a" x y dirs))
         (let ([ret (move (find-weightest-dir dirs))])
           (when (not (= OXYG ret))
             (dump-stage)
             (sleep 0.08)
             (loop)))]))))

(go)

(reset-xy)
(dump-stage)
;TODO: Wall on the road, and try to find all road & wall

;(part1 input)