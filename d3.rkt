#lang racket

(require "input.rkt")

(define wire-1 "R75,D30,R83,U83,L12,D49,R71,U7,L72")
(define wire-2 "U62,R66,U55,R34,D71,R55,D58,R83")

(define lines (input-load-lines 3))
(define input-1 (first lines))
(define input-2 (second lines))

(struct p (raw dir dist x1 y1 x2 y2) #:mutable)

(define (build-paths wire)
  (map (lambda (move)
         (p move
            (string-ref move 0)
            (string->number (substring move 1 (string-length move)))
            0 0 0 0))
       (string-split wire ",")))

(define (path-intersects-xy? p1 p2)
  (and
   (and (> (p-x1 p1) (p-x1 p2))
        (< (p-x1 p1) (p-x2 p2)))
   (and (> (p-y1 p2) (p-y1 p1))
        (< (p-y1 p2) (p-y2 p1)))))

(define (path-intersects? p1 p2)
  (or (path-intersects-xy? p1 p2)
      (path-intersects-xy? p2 p1)))

(path-intersects?
 (p #f #f #f 0 2 10 2)
 (p #f #f #f 3 0 3 5))

(path-intersects?
 (p #f #f #f 0 2 10 2)
 (p #f #f #f 0 5 3 5))

(build-paths wire-1)
  

