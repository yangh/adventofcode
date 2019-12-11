#lang racket

(require "input.rkt")

(define input (input-load-lines "10-1"))

(struct ast (x y k v) #:mutable)

(define (ast=? a b)
  (= (ast-k a) (ast-k b)))

(define (ast->str a)
  (format "[~a] (~a ~a), V: ~a" (ast-k a) (ast-x a) (ast-y a) (ast-v a)))

(define (point-distance x1 y1 x2 y2)
  (let ([a (abs (- x1 x2))]
        [b (abs (- y1 y2))])
    (sqrt (+ a b))))

(define (point-on-line x y x1 y1 x2 y2)
  (and
   (and (>= y (min y1 y2)) (<= y (max y1 y2)))
   (and (>= x (min x1 x2)) (<= x (max x1 x2)))
   (let* ([ab (point-distance x1 y1 x2 y2)]
          [ac (point-distance x y x1 y1)]
          [bc (point-distance x y x2 y2)]
          [margin (abs (- ab ac bc))])
     (displayln (format "Margin: (- ~a ~a ~a) = ~a" ab ac bc margin))
     (> margin 1.0))))

(define (has-ast-in-line a b asts)
  (ormap
   (lambda (c)
     (displayln
      (format "Check: ~a on ~a ~a"
              (ast->str c) (ast->str a) (ast->str b)))
     (and (not (ast=? c a))
          (not (ast=? c b))
          (point-on-line (ast-x c) (ast-y c)
                         (ast-x a) (ast-y a)
                         (ast-x b) (ast-y b))))
   (hash-values asts)))

(define (find-visible-asts a asts)
  (foldl
   (lambda (b r)
     (displayln (format "Check for ast line: ~a, ~a" (ast->str a) (ast->str b)))
     (+ r (if (and (not (ast=? a b)) (not (has-ast-in-line a b asts))) 1 0)))
   0 (hash-values asts)))

(define (test input)
  (define v (length input))
  (define h (string-length (first input)))
  (displayln (format "Input: ~a x ~a" h v))
  (for-each displayln input)

  (define asts (make-hash))
  (for* ([y (range 0 v)]
         [x (range 0 h)])
    (when (char=? #\# (string-ref (list-ref input y) x))
      (displayln (format "Asteroid: ~a, ~a" x y))
      (let ([k (+ x (* x y))])
        (hash-set! asts k (ast
                           (exact->inexact x)
                           (exact->inexact y) k 0)))))

  (for-each
   (lambda (a)
     (set-ast-v! a (find-visible-asts a asts))
     (let ([x (inexact->exact (ast-x a))]
           [y (inexact->exact (ast-y a))])
       (string-set! (list-ref input y) x
                    (string-ref (format "~a" (ast-v a)) 0))))
   (hash-values asts))

  (for-each
   (lambda (a)
     (displayln (format  "Asteroid: ~a" (ast->str a))))
   (hash-values asts))

  (for-each displayln input))

(test input)