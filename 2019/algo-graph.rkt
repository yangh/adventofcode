#lang racket
;
; Utilities for algorithm of graphy
;
(provide (all-defined-out))

; Path --------------------
; paths item (list x y))
(define paths '())
(define (path-reset) (set! paths '()))
(define (path-empty) (= 0 (length paths)))
(define (path-push a) (set! paths (append (list a) paths)))
(define (path-peak) (car paths))
(define (path-peak2) (car (cdr paths)))
(define (path-len) (length paths))
(define (path-dump-len) (displayln (format "Path len: ~a" (path-len))))

(define (path-pop)
  (let ([a (car paths)])
    (set! paths (cdr paths))
    a))

(define (path-equal a b)
  (and (= (list-ref a 0) (list-ref b 0))
       (= (list-ref a 1) (list-ref b 1))))

(define (path-add x y)
  (cond
    [(path-empty) (path-push (list x y))]
    [(and (> (path-len) 1)
          (path-equal (path-peak2) (list x y)))
     ;(displayln (format "Path fallback: ~a ~a" x y))
     (path-pop)]
    [else (path-push (list x y))]))

; ----------------

; Clock wise
(define move-delta1
  ; Move  x,y,direction,index
  (list '(0 -1 1 0) ; North
        '(1  0 4 1) ; East
        '(0  1 2 2) ; South
        '(-1 0 3 3) ; West
        ))

; Counter lock wise
(define move-delta2
  ; Move  x,y,direction,index
  (list '(0 -1 1 0) ; North
        '(-1 0 3 1) ; West
        '(0  1 2 2) ; South
        '(1  0 4 3) ; East
        ))

(define move-delta move-delta1)

(define (set-move-clock-wise)
  (set! move-delta move-delta1))

(define (set-move-cclock-wise)
  (set! move-delta move-delta2))

; -----------------

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

(define h 50)
(define v h)

(define (set-h! n) (set! h n))
(define (set-v! n) (set! v n))

; Stage
(define stage (make-vector (* h v) EMPT))

; How many times we stepped on this point
(define stage-weight2 (make-vector (* h v) 0))
(define stage-weight2-strs (list  "." "A" "B" "C" "D" "E" "F" "G" "H"))

; Set stage data
(define (stage-set! x y c)
  (vector-set! stage (+ x (* h y)) c))

; Find all open dirs in types in a list of
;  '(weight dir)
(define (find-dir-open-at x y dir-types)
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
                 [(member c dir-types) (list (list (+ c w2) dir))]
                 [else '()]))))
   '() move-delta))


(define (dump-stage)
  ; Print cabinet
  (for ([i (range 0 h)]) (display "-"))
  (displayln "")


  (for ([i (range 0 (* h v))])
    (let ([c (vector-ref stage i)])
      (display (list-ref stage-strs c)))
    (when (= (modulo i h) (sub1 h)) (displayln "")))
  (displayln ""))
