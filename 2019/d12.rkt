#lang racket

(require "input.rkt")

(struct moon (px py pz vx vy vz) #:mutable #:transparent)

; Parse input to struct moon
(define (parse-moon line)
  (define l1 (substring line 1 (sub1 (string-length line))))
  (define values
    (map (lambda (str)
           (string->number
            (string-trim (second (string-split str "=")))))
         (string-split l1 ",")))
  (moon (list-ref values 0)
        (list-ref values 1)
        (list-ref values 2)
        0 0 0))

(define moons-original #f)
(define moons #f)

(define (load-data n)
  (map parse-moon (input-load-lines n)))

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
  (displayln (format "Moons at original: ~a" (moons-at-original 0))))

(define (shuffle-x n)
  (define x '())
  (for ([i (range 0 n)])
    (for ([y (range (add1 i) n)])
      ;(displayln (format "~a" (list i y)))
      (set! x (append x (list (list i y))))))
  x)

(define moon-shuffler (shuffle-x 4))

(define (vel-delta x1 x2)
  (cond
    [(> x1 x2) '(-1 1 #t)]
    [(< x1 x2) '(1 -1 #t)]
    ; Add 0 is faster than if/when/else branch
    [else      '(0  0 #f)]))

(define (simulate-motion idxs)
  (let* ([m0 (list-ref moons (list-ref idxs 0))]
         [m1 (list-ref moons (list-ref idxs 1))]
         [x-delta (vel-delta (moon-px m0) (moon-px m1))]
         [y-delta (vel-delta (moon-py m0) (moon-py m1))]
         [z-delta (vel-delta (moon-pz m0) (moon-pz m1))])
    ;(displayln (format "Check ~a, v delta: ~a ~a ~a"
    ;                   idxs x-delta y-delta z-delta))
    (set-moon-vx! m0 (+ (moon-vx m0) (first  x-delta)))
    (set-moon-vy! m0 (+ (moon-vy m0) (first  y-delta)))
    (set-moon-vz! m0 (+ (moon-vz m0) (first  z-delta)))
    (set-moon-vx! m1 (+ (moon-vx m1) (second x-delta)))
    (set-moon-vy! m1 (+ (moon-vy m1) (second y-delta)))
    (set-moon-vz! m1 (+ (moon-vz m1) (second z-delta)))
    ))

(define (moon-update-pos m)
  (set-moon-px! m (+ (moon-px m) (moon-vx m)))
  (set-moon-py! m (+ (moon-py m) (moon-vy m)))
  (set-moon-pz! m (+ (moon-pz m) (moon-vz m))))

; Simulate n steps
(define (simulate n)
  (for ([i (range 0 n)])
    ; Cal velocity
    (for-each simulate-motion moon-shuffler)
    ; Update position by velocity
    (for-each moon-update-pos moons)
    ;(displayln (format "Total energy: ~a" (moon-energy-all moons)))
    ))

(define (moons-at-original dim)
  (and
   ; All velocity is zero
   (andmap
    (lambda (m)
      (cond
        [(= dim 0) (= 0 (moon-vx m))]
        [(= dim 1) (= 0 (moon-vy m))]
        [(= dim 2) (= 0 (moon-vz m))]))
    moons)
   ; All position go to original
   (andmap
    (lambda (m mo)
      (cond
        [(= dim 0) (= (moon-px m) (moon-px mo))]
        [(= dim 1) (= (moon-py m) (moon-py mo))]
        [(= dim 2) (= (moon-pz m) (moon-pz mo))]))
    moons moons-original)))

; Iterate until moons go back to the original position
(define (simulate-to-original dim)
  ; Reset input data
  (set! moons (load-data fn))
  (let loop ([steps 1])
    ; Calculate velocity
    (for-each simulate-motion moon-shuffler)
    ; Update position by velocity
    (for-each moon-update-pos moons)
    ;(displayln (format "Total energy: ~a" (moon-energy-all)))
    (when (= 0 (modulo steps 100000))
      (displayln (format "Iterating step: ~a" steps)))
    (cond
      [(moons-at-original dim)
       (displayln (format "Found steps: ~a" steps))
       steps]
      [else (loop (add1 steps))])))

(define fn "12-1")
(set! moons (load-data fn))
(set! moons-original (load-data fn))
(dump-moons moons-original)

; Part 1
(simulate 1000)
(dump-moons moons)

; Part 2
; Calculate LCM of steps of cycling for each axis
(lcm
 (simulate-to-original 0)
 (simulate-to-original 1)
 (simulate-to-original 2))
(dump-moons moons)