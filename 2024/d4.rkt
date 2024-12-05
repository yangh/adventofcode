#lang racket

(require math/base)
(require "utils.rkt")

(define lines (input-load-lines "4"))

(define xmas (string->list "XMAS"))

(define mr '((0 0)
             (1 0)
             (2 0)
             (3 0)))

(define ml '(( 0 0)
             (-1 0)
             (-2 0)
             (-3 0)))

(define md '((0 0)
             (0 1)
             (0 2)
             (0 3)))

(define mu '((0  0)
             (0 -1)
             (0 -2)
             (0 -3)))

(define seq4  '( 0  1  2  3))
(define zero4 '( 0  0  0  0))
(define pos4  '( 1  1  1  1))
(define neg4  '(-1 -1 -1 -1))

;(map list seq4 zero4)

(define (mull la lb)
  (map * la lb))

(define ne (map list (mull seq4 pos4) (mull seq4 pos4)))
(define nw (map list (mull seq4 neg4) (mull seq4 pos4)))
(define se (map list (mull seq4 pos4) (mull seq4 neg4)))
(define sw (map list (mull seq4 neg4) (mull seq4 neg4)))

(define dir8s (list mr ml md mu ne nw se sw))

(define mrows (length lines))
(define mcols (string-length (first lines)))

(define (valid-pos point)
  (let ([x (first point)]
        [y (second point)])
    (and (>= x 0)
         (>= y 0)
         (< x mcols)
         (< y mrows))))

(define (valid-poses poses)
  (andmap valid-pos poses))

(define (build-poses point mx)
  (let ([x (first point)]
        [y (second point)])

    (map (lambda (m)
           (list (+ x (first m)) (+ y (second m))))
         mx)))

;(build-poses 0 0 mr)
;(build-poses 0 1 mr)
;(valid-poses (build-poses 0 0 mr))
;(valid-poses (build-poses 0 0 ml))

(define (take-word poses)
  ;(displayln poses)
  (map (lambda (pos)
         (string-ref (list-ref lines (second pos)) (first pos)))
       poses))

;lines
;(take-word (build-poses 0 0 mr))

(define (is-xmas word)
  (andmap (lambda (a b)
            (eq? a b))
          word xmas))

;(is-xmas (take-word (build-poses 5 0 mr)))

(define (generate-coordinates width height)
  (for*/list ([x (in-range width)]
              [y (in-range height)])
    (list x y)))

; 2521
(sum (map (lambda (pos)
            ;(displayln pos)
            (count (lambda (poses)
                     ;(displayln poses)
                     (and (valid-poses poses)
                          (is-xmas (take-word poses))))
                   (map (lambda (matrix)
                          (build-poses pos matrix))
                        (list mr ml mu md ne nw se sw))))
          (generate-coordinates mcols mrows)))
        

