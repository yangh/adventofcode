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
       (list (* xbase (/ x1 gcdn)) (* ybase (/ y1 gcdn)))])))

; Return offset of starts around in n step
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
; (poff, line-step-off)
(define stars-around-table
  (map stars-around (range 1 input-max)))

(define stars-around-line-step-table
  (map (λ (t) (map step-on-line t)) stars-around-table))

; Strar weight: 0 visable, 1 invisable
(define (star-is-visable x y)
  (= 0 (stage-weight-get x y)))

; Return starts around pos as list of
; ((x, y) (x-off, y-off))
(define (find-stars-around pos step)
  (foldl append '()
         (map (λ (poff loff)
                (let ([x (+ (first pos) (first poff))]
                      [y (+ (second pos) (second poff))])
                  (if (and (stage-obj-is x y WALL)
                           (star-is-visable x y))
                      (list (list (list x y) poff loff))
                      '())))
              (list-ref stars-around-table (sub1 step))
              (list-ref stars-around-line-step-table (sub1 step))
              )))

(define (check-stars-on-line pline)
  (let* ([pos (first pline)]
         [poff (second pline)]
         [loff (third pline)]
         [xoff (first loff)]
         [yoff (second loff)])
    (let loop ([x (+ (first pos) xoff)]
               [y (+ (second pos) yoff)])
      (when (stage-pos-valid x y)
        (let ([is-star (stage-obj-is x y WALL)])
          (ddisplayln (format "Checkt ~a ~a (~a) for ~a" x y is-star pline))
          (when is-star
            (stage-weight-set! x y 1)))
        (loop (+ x xoff) (+ y yoff))))))

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

           (for-each
            (λ (step)
              (map check-stars-on-line (find-stars-around pos step)))
            (range 1 h))

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
             v))
         paths))

  (apply max (find-m))
  )

(test "10-1")
(test "10-2")
(test "10-3")
(test "10-4")
(test "10-5")
(test "10")
