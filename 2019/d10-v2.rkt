#lang racket

(require "utils.rkt")
(require "algo-graph.rkt")

; Find the step value of x/y on line to reach out
; all the star on the line
(define (step-on-line poff)
  (let* ([xoff  (first poff)]
         [yoff (second poff)]
         [x1 (abs xoff)]
         [y1 (abs yoff)]
         [gcdn (gcd x1 y1)]
         [xbase (if (> xoff 0) 1 -1)]
         [ybase (if (> yoff 0) 1 -1)])
    (cond
      [(= 0 xoff) (list 0 ybase)]
      [(= 0 yoff) (list xbase 0)]
      [(= x1 y1)  (list xbase ybase)]
      [else
       (list (* xbase (/ x1 gcdn))
             (* ybase (/ y1 gcdn)))])))

; Return offset of stars around in n step
;
;   * * * * *  step = 2
;   *       *
;   *   @   *
;   *       *
;   * * * * *
(define (stars-around step)
  (define l '())
  (for* ([a (list (- step) step)]
         [b (range (- step) (add1 step))])
    (when (not (member (list a b) l))
      (set! l (append l (list (list a b)))))
    (when (not (member (list b a) l))
      (set! l (append l (list (list b a))))))
  (sort l (λ (a b)(< (second a) (second b)))))

; Max input size
(define input-max 32)

; Fast lookup table
; (pos-offset, step-on-line)
(define stars-around-table
  (map stars-around (range 1 input-max)))

(define stars-around-line-step-table
  (map (λ (t) (map step-on-line t)) stars-around-table))

; Strar weight: 0 visable, 1 invisable
(define (star-is-visable x y)
  (= 0 (stage-weight-get x y)))

; Return starts around pos as list of
; ((x, y) step-on-line)
(define (find-stars-around pos step)
  (foldl append '()
         (map (λ (poff loff)
                (let ([x (+ (first pos) (first poff))]
                      [y (+ (second pos) (second poff))])
                  (if (and (stage-obj-is x y WALL)
                           (star-is-visable x y))
                      (list (list (list x y) loff))
                      '())))
              (list-ref stars-around-table (sub1 step))
              (list-ref stars-around-line-step-table (sub1 step))
              )))

; Check and mark star as invisible if it's blocked
;  (behind the first visible start in the same line)
(define (check-stars-on-line pline)
  (let* ([pos  (first  pline)]
         [loff (second pline)]
         [xoff (first  loff)]
         [yoff (second loff)])
    (let loop ([x (+ (first pos) xoff)]
               [y (+ (second pos) yoff)])
      (when (stage-pos-valid x y)
        (let ([is-star (stage-obj-is x y WALL)])
          (ddisplayln (format "Checkt ~a ~a (~a) for ~a" x y is-star pline))
          (when is-star
            (stage-weight-set! x y 1)))
        (loop (+ x xoff) (+ y yoff))))))

; Max step to check
(define (max-step pos)
  (let ([x (abs (first pos))]
        [y (abs (second pos))])
    (max x y (- h x) (- v y))))

(define (test in)
  (define input (input-load-lines in))
  (define v (length input))
  (define h (string-length (first input)))
  (displayln (format "Input: ~a x ~a" h v))

  (set-h! h)
  (set-v! v)
  (stage-reset)
  (path-reset)

  ; Load input into stage
  (for* ([y (range 0 v)]
         [x (range 0 h)])
    (let ([c (list-ref (string->list (list-ref input y)) x)])
      ;(displayln (format "~a ~a: ~a" x y c))
      (stage-set-char! x y c)
      (when (char=? c #\#)
        (path-add x y))))

  ;(dump-stage)

  (define (find-m)
    (map (λ (pos)
           (stage-weight-reset)

           ;(displayln (format "Max step: ~a" (max-step pos)))
           (for-each
            (λ (step)
              (map check-stars-on-line (find-stars-around pos step)))
            (range 1 (max-step pos)))

           ; Strar weight: 0 visable, 1 invisable
           (let ([v (- (path-len) (stage-weight-total) 1)])
             (when #f
               (when (path-equal pos '(11 13))
                 (displayln (format "Check for start ~a, ~a" pos v))
                 (for-each (λ (p)
                             (let ([x (first p)]
                                   [y (second p)])
                               (when (= 0 (stage-weight-get x y))
                                 (stage-weight-set! x y 2))))
                           paths)
                 (stage-weight-set! (first pos) (second pos) 3)
                 (dump-stage-weight)))
             (list v pos)))
         paths))

  ;(apply max (find-m))
  (first (sort (find-m) (λ (a b) (> (first a) (first b)))))
  )

;(test "10-1")
;(test "10-2")
;(test "10-3")
;(test "10-4")
;(test "10-5")

; Part 1
(define m-station (test "10"))
(displayln (format "Monitor station: ~a" m-station))

; Part 2
(define (distance p1 p2)
  (let ([x1 (first p1)]
        [y1 (second p1)]
        [x2 (first p2)]
        [y2 (second p2)])
    (sqrt (+ (* (- x2 x1) (- x2 x1))
             (* (- y2 y1) (- y2 y1))))))

;(distance (list 0 0) (list 3 4))
;(distance (list 11 11) (list 11 0))

(define (xarc p1 p2 which arcvf)
  (let  ([x1 (first p1)]
         [y1 (second p1)]
         [x2 (first p2)]
         [y2 (second p2)])
    (cond
      [(which x1 y1 x2 y2)
       (arcvf x1 y1 x2 y2)]
      [else -1])))

(define (stars-in-which which arcvf)
  (sort
   (filter (λ (e) (>= (first e) 0))
           (let ([m-pos (second m-station)])
             (map (λ (p) (list (xarc m-pos p which arcvf) (distance m-pos p) p))
                  (remove m-pos paths))))
   (λ (a b)
     (cond
       [(= (first a) (first b))
        ; 2nd order by distance
        (< (second a) (second b))]
       [else
        ; 1st order by tangent
        (< (first a) (first b))]))))

; Calculate tangent, distance of stars in each quadrant
;  return a sorted list by quadrant/tangent/distance
;     y^
;      | a
;      |---. p
;      |  /
;    b | / c (distance)
;      |/
;      +-------> x
;
(define stars-in-circle-order
  (append
   ; NE
   (stars-in-which (λ (x1 y1 x2 y2) (and (>= x2 x1) (> y1 y2)))
                   (λ (x1 y1 x2 y2) (abs (/ (- x2 x1) (- y2 y1)))))
   ; SE
   (stars-in-which (λ (x1 y1 x2 y2) (and (>= y2 y1) (> x2 y1)))
                   (λ (x1 y1 x2 y2) (abs (/ (- y2 y1) (- x2 x1)))))
   ; SW
   (stars-in-which (λ (x1 y1 x2 y2) (and (<= x2 x1) (< y1 y2)))
                   (λ (x1 y1 x2 y2) (abs (/ (- x2 x1) (- y2 y1)))))
   ; NW
   (stars-in-which (λ (x1 y1 x2 y2) (and (<= y2 y1) (< x2 y1)))
                   (λ (x1 y1 x2 y2) (abs (/ (- y2 y1) (- x2 x1)))))))

; Vaporize star in clock wise order
(let loop ([idx 0]
           [count 1]
           [prev -1]
           [l stars-in-circle-order])
  (let ([s (list-ref l idx)])
    (cond
      [(= prev (first s))
       (loop (add1 idx) count prev l)]
      [(= count 200)
       (displayln (format "200th: ~a = ~a" s (+ (* 100 (first (third s))) (second (third s)))))]
      [else
       (ddisplayln (format "vaporized: ~a ~a ~a ~a" idx count prev s))
       (stage-set! (first (third s)) (second (third s)) OXYG)
       ;(dump-stage 0.3)
       (loop idx (add1 count) (first s) (remove s l))])))