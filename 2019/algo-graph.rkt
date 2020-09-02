#lang racket
;
; Utilities for algorithm of graphy
;
;   1. Path Stack API
;   2. Find open paths, select by weight
;   3. ...
;
; Used in day 17
;
(provide (all-defined-out))

; Path --------------------
; paths item (list x y))
(define paths '())
(define (path-reset) (set! paths '()))
(define (path-empty?) (= 0 (length paths)))
(define (path-push a) (set! paths (append (list a) paths)))
(define (path-remove p) (set! paths (remove p paths)))
(define (path-peak) (car paths))
(define (path-peak2) (car (cdr paths)))
(define (path-len) (length paths))
(define (path-dump-len) (displayln (format "Path len: ~a" (path-len))))
(define (path-has p) (member p paths))

(define (path-pop)
  (if (empty? paths) #f
      (let ([a (car paths)])
        (set! paths (cdr paths))
        a)))

(define (path-equal a b)
  (and (= (list-ref a 0) (list-ref b 0))
       (= (list-ref a 1) (list-ref b 1))))

(define (path-add x y) (path-push (list x y)))

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

; Return move delta of a dir
(define (move-delta-of-dir dir)
  (let ([deltas (filter (λ (m) (= dir (list-ref m 2))) move-delta)])
    (cond
      [(= 1 (length deltas))
       (first deltas)]
      [else
       (displayln (format "Unkown dir: ~a, ~a" dir deltas))
       #f])))

(define (set-move-clock-wise)
  (set! move-delta move-delta1))

(define (set-move-cclock-wise)
  (set! move-delta move-delta2))

; Choose move back direction
(define (get-move-back-dir dir)
  (let ([idx (list-ref dir 3)])
    (list-ref move-delta (modulo (+ idx 2) 4))))

; -----------------

; A Droid on the stage, with it's own direction
(struct droid (x y dir) #:mutable #:transparent)

; Direction
(define NORTH 1) ; North
(define SOUTH 2) ; South
(define WEST  3) ; West
(define EAST  4) ; East

; Stage blocks index
(define WALL 0)
(define ROAD 1)
(define OXYG 2)
(define DROD 3)
(define EMPT 4)
(define stage-strs (list  "#" "." "@" "D" " "))

(define (char->block c)
  (cond
    [(char=? c #\#) WALL]
    [(char=? c #\.) ROAD]
    [else EMPT]))

; Width, Height
(define init-h 50)
(define h init-h)
(define v h)

(define (set-h! n) (set! h n))
(define (set-v! n) (set! v n))

; Current droid position
(define dx 0)
(define dy 0)
(define (set-current-droid-pos! x y)
  (set! dx x)
  (set! dy y))

; Stage
(define stage (make-vector (* h v) EMPT))

(define (stage-init init-h init-v c)
  (set! h init-h)
  (set! v init-v)
  (set! stage (make-vector (* h v) c)))

; How many times we stepped on this point
(define stage-weight (make-vector (* h v) 0))
(define stage-weight-strs (list  "." "A" "B" "C" "D" "E" "F" "G" "H"))
(define (stage-weight-sub1 x y)
  (let ([idx (+ x (* h y))])
    (vector-set! stage-weight idx
                 (sub1 (vector-ref stage-weight idx)))))

(define (stage-weight-get x y)
  (vector-ref stage-weight (+ x (* y h))))

(define (stage-weight-set! x y w)
  (let ([idx (+ x (* h y))])
    (vector-set! stage-weight idx w)))

(define (stage-weight-reset)
  (vector-fill! stage-weight 0))

(define (stage-reset)
  (vector-fill! stage EMPT))

; Set stage data
(define (stage-set! x y c)
  (when (stage-pos-valid x y)
    (vector-set! stage (+ x (* h y)) c)))

(define (stage-set-all! c)
  (vector-fill! stage c))

(define (stage-set-char! x y c)
  (stage-set! x y (char->block c)))

(define (stage-pos-valid x y)
  (and (>= x 0) (< x h)
       (>= y 0) (< y v)))

(define (stage-obj-is x y obj)
  (and (>= x 0) (< x h)
       (>= y 0) (< y v)
       (= obj (vector-ref stage (+ x (* y h))))))

; Find all open dirs in types in a list of
;  '(weight dir)
; TODO: Check if x/y is valid
(define (find-dir-open-at x y dir-types)
  (filter-map
   (lambda (dir)
     (let* ([nx (+ x (first dir))]
            [ny (+ y (second dir))]
            [idx (+ nx (* ny h))]
            [c (vector-ref stage idx)]
            [w2 (vector-ref stage-weight idx)])
       ;(ddisplayln (format "Find ~a ~a ~a ~a" nx ny c w2))
       (and (member c dir-types) (list (+ c w2) dir))))
   move-delta))

; Select weightest way
(define (find-weightest-dir dirs)
  (define dir (first dirs))
  (for-each
   (lambda (d)
     (when (> (first d) (first dir))
       (set! dir d)))
   (cdr dirs))
  (second dir))

; Stage weight sum
(define (stage-weight-total)
  (foldl + 0 (vector->list stage-weight)))

(define (dump-stage [delay 0] [show-droid #f])
  ; Print cabinet
  (for ([i (range 0 h)]) (display "-"))
  (displayln "")

  (let ([cidx (+ dx (* dy h))])
    (for ([i (range 0 (* h v))])
      (let ([c (vector-ref stage i)])
        (if (and show-droid (= cidx i))
            (display "D")
            (display (list-ref stage-strs c))))
      (when (= (modulo i h) (sub1 h)) (displayln ""))))
  (displayln "")
  (flush-output)
  (sleep delay))


(define (dump-stage-weight)
  (displayln "Dump stage weight")
  ; Print cabinet
  (for ([i (range 0 h)]) (display "-"))
  (displayln "")

  (for ([i (range 0 (* h v))])
    (let ([c (vector-ref stage-weight i)])
      (display (list-ref stage-weight-strs (abs c))))
    (when (= (modulo i h) (sub1 h)) (displayln "")))
  (displayln ""))
