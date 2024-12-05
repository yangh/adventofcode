#lang racket

(require "utils.rkt")

(define lines (input-load-lines "2"))

(define reports (map (lambda (l) (map string->number (string-split l))) lines))

(define DECR 0)
(define INCR 1)
(define INVAL 2)

(define (get-creasement l)
  (let ([steps (- (first l) (second l))])
    ;(displayln (format "steps ~a" steps))
    (cond ([> steps 0] DECR)
          ([< steps 0] INCR)
          (else INVAL))))

(define (valid-steps steps)
      (and ;(> steps 0)
        (>= steps 1)
        (<= steps 3)))

(define (canonical report)
  (let ([dir (get-creasement report)])
    ;(displayln report)
    ;(displayln (format "dir ~a" dir))

    (and (not (= INVAL dir))
         (let loop ([levels report])
           (or (= 1 (length levels)) ; last one
               (and (valid-steps
                      (cond ([= DECR dir] (- (first levels) (second levels)))
                            ([= INCR dir] (- (second levels) (first levels)))))
                    (loop (rest levels))))))))

; 549
(count canonical reports)

(define (shuff-n l)
  (map (lambda (n)
         ;(displayln (format "n: ~a, ~a ~a" n (take l n) (take-right l (- (length l) n))))
         (append (take l n) (take-right l (- (length l) n 1))))
       (range 0 (length l))))

;(define n5 '(1 2 3 4 5))
;(shuff-n n5)

(define (canonical-shuff report)
  (or (canonical report)
       (> (count canonical (shuff-n report)) 0)))

; 589
(count canonical-shuff reports)
