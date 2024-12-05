#lang racket

(require math/base)
(require "utils.rkt")

(define lines (input-load-lines "1"))

(define l1 (make-vector (length lines) 0))
(define l2 (make-vector (length lines) 0))

(define index 0)

(for-each (lambda (l)
            (let* ([strs (string-split l "   ")])
              ;(displayln strs)
              (vector-set! l1 index (string->number (first strs)))
              (vector-set! l2 index (string->number (second strs)))
              (set! index (+ index 1))))
          lines)

(define (total-distance)
  (define l11 (vector-sort l1 <))
  (define l22 (vector-sort l2 <))

  (define total 0)
  (for ([v1 (vector->list l11)]
        [v2 (vector->list l22)])
    ;(displayln (format "~a ~a dist ~a" v1 v2 (abs (- v1 v2))))
    (set! total (+ total (abs (- v1 v2)))))
  total
  )

; 1970720
(total-distance)

(foldr (lambda (v1 v2 t)
         (+ t (abs (- v1 v2))))
       0
       (vector->list (vector-sort l1 <))
       (vector->list (vector-sort l2 <)))

(sum (map (lambda (v1 v2)
            (abs (- v1 v2)))
          (vector->list (vector-sort l1 <))
          (vector->list (vector-sort l2 <))))

(define (similarity-score)
  (define total 0)
  (for ([v1 (vector->list l1)])
    (let ([count 0])
      (for ([v2 (vector->list l2)])
        (when (= v1 v2)
          (set! count (add1 count)))
        )
      (set! total (+ total (* v1 count)))
      ))
  total)

; 17191599
(similarity-score)

(foldr (lambda (v1 t)
         (+ t (* v1 (foldr (lambda (v2 t)
                             (+ t (if (= v1 v2) 1 0)))
                           0 (vector->list l2)))))
       0 (vector->list l1))

(sum (map (lambda (v1)
            (* v1 (sum (map (lambda (v2)
                              (if (= v1 v2) 1 0))
                            (vector->list l2)))))
            (vector->list l1)))