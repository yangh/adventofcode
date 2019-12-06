#lang racket

(define (password-p1? n)
  (let* ([ns (string->list (format "~a" n))]
         [n1 (list-ref ns 0)]
         [n2 (list-ref ns 1)]
         [n3 (list-ref ns 2)]
         [n4 (list-ref ns 3)]
         [n5 (list-ref ns 4)]
         [n6 (list-ref ns 5)])
    (and
     (and (char<=? n1 n2)
          (char<=? n2 n3)
          (char<=? n3 n4)
          (char<=? n4 n5)
          (char<=? n5 n6))
     (or (char=? n1 n2)
         (char=? n2 n3)
         (char=? n3 n4)
         (char=? n4 n5)
         (char=? n5 n6)))))

(define (count start end password?)
  (foldl + 0
         (map (lambda (n)
                ;(when (password? n) (displayln n))
                (if (password? n) 1 0))
              (range start end))
         ))

;(count 254032 789860)
;(count 254032 789860 password-p1?)

(define (password-p2? n)
  (let* ([ns (string->list (format "~a" n))]
         [n1 (list-ref ns 0)]
         [n2 (list-ref ns 1)]
         [n3 (list-ref ns 2)]
         [n4 (list-ref ns 3)]
         [n5 (list-ref ns 4)]
         [n6 (list-ref ns 5)])
    (and
     (and (char<=? n1 n2)
          (char<=? n2 n3)
          (char<=? n3 n4)
          (char<=? n4 n5)
          (char<=? n5 n6))
     (or (and (char=? n1 n2) (not (char=? n2 n3)))
         (and (char=? n2 n3)
              (and (not (char=? n1 n2))
                   (not (char=? n3 n4))))
         (and (char=? n3 n4)
              (and (not (char=? n2 n3))
                   (not (char=? n4 n5))))
         (and (char=? n4 n5)
              (and (not (char=? n3 n4))
                   (not (char=? n5 n6))))
         (and (char=? n5 n6) (not (char=? n4 n5)))))))

(count 254032 789860 password-p2?)