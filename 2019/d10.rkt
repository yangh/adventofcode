#lang racket

(require "input.rkt")

(struct ast (x y k v) #:mutable)

(define (ast=? a b) (= (ast-k a) (ast-k b)))

(define (ast->str a)
  (format "[~a] (~a ~a), V: ~a" (ast-k a) (ast-x a) (ast-y a) (ast-v a)))

(define (elipson x1 y1 x2 y2)
  (let ([dx1 (- x2 x1)]
        [dy1 (- y2 y1)])
    (* 0.003 (+ (* dx1 dx1) (* dy1 dy1)))))

(define (point-on-line-by-pdp x y x1 y1 x2 y2)
  (let ([pdp (- (* (- x1 x) (- y2 y)) (* (- y1 y) (- x2 x)))]
        [eli (elipson x1 y1 x2 y2)])
    (displayln (format "PDP: ~a, elipson: ~a, (~a ~a) to (~a ~a) (~a ~a)"
                       pdp eli x y x1 y1 x2 y2))
    (< (abs pdp) eli)
    ))

;(point-on-line-by-pdp 1.0 0 2.0 2.0 3.0 4.0)
;(point-on-line-by-pdp 4.0 0 2.0 2.0 3.0 4.0)
;(point-on-line-by-pdp 1.0 0 1.0 2.0 3.0 4.0)

(define (point-on-line x y x1 y1 x2 y2)
  (and
   (and (>= y (min y1 y2)) (<= y (max y1 y2)))
   (and (>= x (min x1 x2)) (<= x (max x1 x2)))
   (point-on-line-by-pdp x y x1 y1 x2 y2)))

(define (has-ast-in-line a b asts)
  (ormap
   (lambda (c)
     (and (not (ast=? c a))
          (not (ast=? c b))
          (point-on-line (ast-x c) (ast-y c)
                         (ast-x a) (ast-y a)
                         (ast-x b) (ast-y b))))
   (hash-values asts)))

(define (find-visible-asts a asts)
  (foldl
   (lambda (b r)
     (displayln (format "Line: ~a, ~a" (ast->str a) (ast->str b)))
     (+ r (if (and (not (ast=? a b)) (not (has-ast-in-line a b asts))) 1 0)))
   0 (hash-values asts)))

(define (test n)
  (define input (input-load-lines n))
  (define v (length input))
  (define h (string-length (first input)))
  (displayln (format "Input: ~a x ~a" h v))
  (for-each displayln input)

  (define asts (make-hasheqv))
  (for* ([y (range 0 v)]
         [x (range 0 h)])
    (when (char=? #\# (string-ref (list-ref input y) x))
      (let ([k (+ x (* x y))])
        (displayln (format "Asteroid: [~a] ~a, ~a" k x y))
        (hash-set! asts k (ast
                           (exact->inexact x)
                           (exact->inexact y) k 0)))))

  (define ma (ast 0 0 0 0))
  (for-each
   (lambda (a)
     (set-ast-v! a (find-visible-asts a asts))
     (when (> (ast-v a) (ast-v ma))
       (set! ma a)))
   (hash-values asts)
   ;(list (hash-ref asts 15))
   )
  (let ([x (inexact->exact (ast-x ma))]
        [y (inexact->exact (ast-y ma))])
    (string-set! (list-ref input y) x #\@))

  (for-each
   (lambda (a)
     (displayln (format  "Asteroid: ~a" (ast->str a))))
   (hash-values asts))

  (for-each displayln input))

(test "10-1")
