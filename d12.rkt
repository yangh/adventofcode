#lang racket

(require "input.rkt")

(struct moon (px py pz vx vy vz) #:mutable #:transparent)

; Parse input to struct moon
(define (parse-moon line)
  (define l1 (substring line 1 (sub1 (string-length line))))
  (define values
    (map (lambda (str)
           (string->number
            (string-trim (list-ref (string-split str "=") 1))))
         (string-split l1 ",")))
  (moon (list-ref values 0)
        (list-ref values 1)
        (list-ref values 2)
        0 0 0))

(define moons-original #f)
(define moons #f)

(define (load-data n)
  (map parse-moon (input-load-lines n)))

(define (moon-energy-kinetic m)
  (+ (abs (moon-vx m))
     (abs (moon-vy m))
     (abs (moon-vz m))))

(define (moon-energy m)
  (* (+ (abs (moon-px m))
        (abs (moon-py m))
        (abs (moon-pz m)))
     (+ (abs (moon-vx m))
        (abs (moon-vy m))
        (abs (moon-vz m)))))

(define (moon-energy-all ms)
  (foldl + 0 (map moon-energy ms)))

(define (dump-moons ms)
  (for-each
   (lambda (m) (displayln (format "~a" m))) ms)
  (displayln (format "Total energy: ~a" (moon-energy-all ms)))
  (displayln (format "Moons at original: ~a" (moons-at-original))))

(define (shuffle-x n)
  (define x '())
  (for ([i (range 0 n)])
       (for ([y (range (add1 i) n)])
            ;(displayln (format "~a" (list i y)))
            (set! x (append x (list (list i y))))))
  x)

(define shuffler (shuffle-x 4))

(define (vel-off x1 x2)
  (cond
    [(> x1 x2) '(-1 1 #t)]
    [(< x1 x2) '(1 -1 #t)]
    [else      '(0  0 #f)]))

(define (simulate-motion idxs)
  (let* ([m0 (list-ref moons (list-ref idxs 0))]
         [m1 (list-ref moons (list-ref idxs 1))]
         [x-off (vel-off (moon-px m0) (moon-px m1))]
         [y-off (vel-off (moon-py m0) (moon-py m1))]
         [z-off (vel-off (moon-pz m0) (moon-pz m1))])
    ;(displayln (format "Check ~a, v off: ~a ~a ~a"
    ;                   idxs x-off y-off z-off))
    (set-moon-vx! m0 (+ (moon-vx m0) (first  x-off)))
    (set-moon-vy! m0 (+ (moon-vy m0) (first  y-off)))
    (set-moon-vz! m0 (+ (moon-vz m0) (first  z-off)))
    (set-moon-vx! m1 (+ (moon-vx m1) (second x-off)))
    (set-moon-vy! m1 (+ (moon-vy m1) (second y-off)))
    (set-moon-vz! m1 (+ (moon-vz m1) (second z-off)))
    ))

(define (moon-update-pos m)
  (set-moon-px! m (+ (moon-px m) (moon-vx m)))
  (set-moon-py! m (+ (moon-py m) (moon-vy m)))
  (set-moon-pz! m (+ (moon-pz m) (moon-vz m))))

; Simulate n steps
(define (simulate n)
  (for ([i (range 0 n)])
       ; Cal velocity
       (for-each simulate-motion shuffler)
       ; Update position by velocity
       (for-each moon-update-pos moons)
       (displayln (format "Total energy: ~a" (moon-energy-all moons)))
       ))

(define (moons-at-original)
  (and
   ; All velocity is zero
   (andmap
    (lambda (m)
      (and (= 0 (moon-vx m))
           (= 0 (moon-vy m))
           (= 0 (moon-vz m))))
    moons)
   (andmap
    ; All position go to original
    (lambda (m mo)
      (and (= (moon-px m) (moon-px mo))
           (= (moon-py m) (moon-py mo))
           (= (moon-pz m) (moon-pz mo))))
    moons moons-original)))

; Iterate untial moons go back to original position
(define (simulate-to-original)
  (let loop ([steps 1])
    ; Cal velocity
    (for-each simulate-motion shuffler)
    ; Update position by velocity
    (for-each moon-update-pos moons)
    ;(displayln (format "Total energy: ~a" (moon-energy-all)))
    (when (= 0 (modulo steps 1000000))
      (displayln (format "Iterator step: ~a" steps)))
    (if (moons-at-original)
        (displayln (format "Found steps: ~a" steps))
        (loop (add1 steps)))
    ))

(define fn "12-e1")
(set! moons (load-data fn))
(set! moons-original (load-data fn))

(simulate 2772)
;(simulate-to-original)
(dump-moons moons)
(dump-moons moons-original)
