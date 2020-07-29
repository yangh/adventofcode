#lang racket

(require "input.rkt")

(struct moon (px py pz vx vy vz) #:mutable #:transparent)

(define input-data (input-load-lines "12-e1"))

(define (parse-moon line)
  (define l1 (substring line 1 (sub1 (string-length line))))
  (define values
    (map (lambda (str)
           (string->number (string-trim (list-ref (string-split str "=") 1))))
         (string-split l1 ",")))
  (moon (list-ref values 0)
        (list-ref values 1)
        (list-ref values 2)
        0 0 0))

(define moons (map parse-moon input-data))

(define (moon-energy m)
  (* (+ (abs (moon-px m))
        (abs (moon-py m))
        (abs (moon-pz m)))
     (+ (abs (moon-vx m))
        (abs (moon-vy m))
        (abs (moon-vz m)))))

(define (moon-energy-all)
  (foldl + 0
         (map moon-energy moons)))

(define (dump-moons)
  (for-each (lambda (m)
              (displayln (format "~a" m)))
            moons)
  (displayln (format "Total energy: ~a"
                     (moon-energy-all))))

(define (shuffle-x n)
  (define x '())
  (for ([i (range 0 n)])
    (for ([y (range (add1 i) n)])
      ;(displayln (format "~a" (list i y)))
      (set! x (append x (list (list i y))))
      ))
  x)

(define shuffler (shuffle-x 4))

(define (vel-off x1 x2)
  (cond
    [(> x1 x2) '(-1 1)]
    [(< x1 x2) '(1 -1)]
    [else      '(0  0)]))

(define (simulate-motion idxs)
  (let* ([m0 (list-ref moons (list-ref idxs 0))]
         [m1 (list-ref moons (list-ref idxs 1))]
         [x-off (vel-off (moon-px m0) (moon-px m1))]
         [y-off (vel-off (moon-py m0) (moon-py m1))]
         [z-off (vel-off (moon-pz m0) (moon-pz m1))])
    (displayln (format "Check ~a, v off: ~a ~a ~a"
                       idxs x-off y-off z-off))
    (set-moon-vx! m0 (+ (moon-vx m0) (first  x-off)))
    (set-moon-vx! m1 (+ (moon-vx m1) (second x-off)))
    (set-moon-vy! m0 (+ (moon-vy m0) (first  y-off)))
    (set-moon-vy! m1 (+ (moon-vy m1) (second y-off)))
    (set-moon-vz! m0 (+ (moon-vz m0) (first  z-off)))
    (set-moon-vz! m1 (+ (moon-vz m1) (second z-off)))
    ))

(define (simulate n)
  (for ([i (range 0 n)])
    ; Cal velocity
    (for-each simulate-motion shuffler)
    ; Update position by velocity
    (for-each
     (lambda (m)
       (set-moon-px! m (+ (moon-px m) (moon-vx m)))
       (set-moon-py! m (+ (moon-py m) (moon-vy m)))
       (set-moon-pz! m (+ (moon-pz m) (moon-vz m))))
     moons)
    ))

(simulate 10)
(dump-moons)