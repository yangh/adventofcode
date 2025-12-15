#lang racket

(require plot)

; Load input as list of lines
(define (input-load-lines d)
  (port->lines (open-input-file (format "inputs/d~a.txt" d))))

(define input-lines (input-load-lines 9))

(define nums (map (lambda (l)
                    (map string->number
                         (string-split l ",")))
                  input-lines))

(define x-min (apply min (map car nums)))
(define x-max (apply max (map car nums)))
(define y-min (apply min (map cadr nums)))
(define y-max (apply max (map cadr nums)))

(displayln (format "~a ~a ~a ~a" x-min x-max y-min y-max))

(define nums-small (map (lambda (l)
                          (map (lambda (n) (/ n 100000.0)) l))
                        nums))

(define prenders
  (list
   (points 
    nums

    #:line-width 0.1
    #:x-min (- x-min)
    #:x-max (+ x-min x-max)
    #:y-min (- y-min)
    #:y-max (+ y-min y-max)
    )
   (lines
    ;; concat head & tail
    (append nums (list (car nums)))
    #:width 0.2
    #:color 'blue)
   ))

(define d9p
  (parameterize ([plot-width    400]
                 [plot-height   400]
                 [plot-x-label  #f]
                 [plot-y-label  #f])
    (list (plot prenders)
          )))

(define (p2f)
  (plot-file (list prenders)
             "d9p.svg"
             #:width 10000
             #:height 10000))

;(p2f)
d9p