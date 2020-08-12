#lang racket

(require "input.rkt")

(define lines (input-load-lines 3))
(define input-1 (first lines))
(define input-2 (second lines))

(struct p (raw dir dist x1 y1 x2 y2) #:mutable)

(define (format-p p)
  (format "~a ~a (~a ~a) (~a ~a)"
          (p-dir p) (p-dist p)
          (p-x1 p) (p-y1 p)
          (p-x2 p) (p-y2 p)))

(define (build-paths wire)
  (list->vector
   (map (lambda (move)
          (p move
             (string-ref move 0)
             (string->number (substring move 1 (string-length move)))
             0 0 0 0))
        (string-split wire ","))))

(define (line-cross? p1 p2)
  (and
   ; x1 > x1' && x1 < x2'
   (and (> (p-x1 p1) (min (p-x1 p2) (p-x2 p2)))
        (< (p-x1 p1) (max (p-x1 p2) (p-x2 p2))))
   ; y1' > y1 && y1' < y2
   (and (> (p-y1 p2) (min (p-y1 p1) (p-y2 p1)))
        (< (p-y1 p2) (max (p-y1 p1) (p-y2 p1))))))
  
(define (path-intersects-xy? p1 p2)
  (or
   (line-cross? p1 p2)
   (line-cross? p2 p1)))

(define (path-intersects? p1 p2)
  (or (path-intersects-xy? p1 p2)
      (path-intersects-xy? p2 p1)))

(path-intersects?
 (p #f #f #f 0 2 10 2)
 (p #f #f #f 3 0 3 5))

(path-intersects?
 (p #f #f #f 0 2 10 2)
 (p #f #f #f 0 5 3 5))

(define (update-aix paths)
  (let loop ([i 0] [x 0] [y 0])
    (let* ([p (vector-ref paths i)]
           [dir (p-dir p)]
           [dist (p-dist p)])
      (set-p-x1! p x)
      (set-p-y1! p y)
      (cond
        [(char=? dir #\U)
         (set-p-x2! p x)
         (set-p-y2! p (+ y dist))]
        [(char=? dir #\D)
         (set-p-x2! p x)
         (set-p-y2! p (- y dist))]
        [(char=? dir #\R)
         (set-p-y2! p y)
         (set-p-x2! p (+ x dist))]
        [(char=? dir #\L)
         (set-p-y2! p y)
         (set-p-x2! p (- x dist))])
      (when (< i (- (vector-length paths) 1))
        (loop (add1 i) (p-x2 p) (p-y2 p))))))


(define (manhattan-distance p1 p2)
  (let ([x (if (= (p-x1 p1) (p-x2 p1))
               (p-x1 p1)
               (p-x1 p2))]
        [y (if (= (p-y1 p1) (p-y2 p1))
               (p-y1 p1)
               (p-y1 p2))])
    (values (+ (abs x) (abs y)) x y)))

(define (p-to-joint-dist p x y)
  (abs
   (if (= x (p-x1 p))
       (- y (p-y1 p))
       (- x (p-x1 p)))))

(define (paths-dump paths)
  (for-each
   (lambda (p)
     (displayln
      (format-p p)))
   (vector->list paths)))

(define (find-nearest-m-dist w1 w2)
  (define paths-1 (build-paths w1))
  (define paths-2 (build-paths w2))

  (update-aix paths-1)
  (update-aix paths-2)
  (paths-dump paths-1)
  (paths-dump paths-2)

  (define min-dist 1000000)
  (define min-plen 1000000)

  (define path-len-1 0)
  (define path-len-2 0)
  (for ([p paths-1])
    (set! path-len-2 0) ; reset
    (for ([q paths-2])
      ;(displayln (format "plen: ~a ~a" path-len-1 path-len-2))
      (when (path-intersects? p q)
        (let*-values ([(d x y) (manhattan-distance p q)]
                      [(plen) (+ path-len-1 path-len-2
                                (p-to-joint-dist p x y)
                                (p-to-joint-dist q x y))])
          (displayln (format "Cross: ~a ~a, mdist: ~a, path-len: ~a"
                             (format-p p) (format-p q) d plen))
          (when (> d 0)
            (when (< d min-dist)
              (set! min-dist d)))
          (when (< plen min-plen)
            (set! min-plen plen))))
      (set! path-len-2 (+ path-len-2 (p-dist q))))
    (set! path-len-1 (+ path-len-1 (p-dist p))))
  (displayln (format "Nearest distance: ~a" min-dist))
  (displayln (format "Mini path distance: ~a" min-plen)))

;(find-nearest-m-dist "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83")
;(find-nearest-m-dist "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

(find-nearest-m-dist "R8,U5,L5,D3" "U7,R6,D4,L4")

; Part 1/2
(find-nearest-m-dist input-1 input-2)