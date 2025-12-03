(use-modules (adv utils))
(use-modules (srfi srfi-1))

(define inputs (load-input "d2.txt"))
(define ranges (string-split (car inputs) #\,))
;;(pp ranges)

(define nranges
  (map (lambda(r)
         (let ((rr (string-split r #\-)))
           (map string->number rr)))
       ranges))
;;(pp nranges)

;; simlair to string-length
(define (num-length n)
  (let loop ((i n))
    (if (< i 10) 1
        (+ 1 (loop (floor-quotient i 10))))))

;; ABAB, ABCABC patten
(define (ntwins? num)
  (let ((len (num-length num)))
    (and (even? len)
         (let* ((nten (expt 10 (/ len 2)))
                (n1 (floor-quotient num nten))
                (n2 (modulo num nten)))
           (= n1 n2)))))

;; Split left to right
;;   ABABABA
;; ->AB AB AB A
;;
;; exp-len (num-length num)
;; exp-n (num-length AB)
;;
(define (num-split exp-len exp-n num)
  (let loop ((n num)
             (exp (- exp-len exp-n)))
    ;;(pp (list "split" n exp exp-len))
    (if (< exp 1)
        (list n)
        (let ((nten (expt 10 exp)))
          (cons (floor-quotient n nten)
                (loop (modulo n nten) (- exp exp-n)))))
    ))
;;(pp (num-split 10 12345))
;;(pp (num-split 100 12345))

(define (num-list-equal? lst)
  (dd (list "test" lst))
  (let loop ((n (car lst) )
             (nlist (cdr lst)))
    (cond
     ((null? nlist) #t)
     ((not (= n (car nlist))) #f)
     (else (and (loop (car nlist) (cdr nlist)))))))

;;(pp (num-list-equal? (num-split 10 12345)))
;;(pp (num-list-equal? (num-split 10 11111)))
;;(pp (num-list-equal? (num-split 100 121212)))

;; ABABAB* pattern
(define (nntwins? num)
  (dd (list "num" num))
  (let* ((len (num-length num))
         (hflen (floor-quotient len 2)))
    (let loop ((exp hflen))
      ;;(pp (list "check" exp num))
      (cond
       ((= exp 0) #f)
       ((num-list-equal? (num-split len exp num))
        ;;(pp (list num exp))
        #t)
       (else
        (or (loop (- exp 1))))))))

;;(pp (nntwins? 12341234))
;;(pp (nntwins? 121212))
;;(pp (nntwins? 20202))
;;(pp (nntwins? 202022))
(tt (nntwins? 20000))
(tt (nntwins? 200000))

;; is-twins?      proc to test number
;; check-all-odd? all numbers are odd?
(define (find-twins is-twins? lst check-all-odd?)
  (dd lst)
  (cond
   ((and check-all-odd?
         (and-map odd? (map num-length lst)))
    (dd "skip all odd length numbers") 0)
   (else
    (let loop ((n1 (car lst))
               (n2 (car (cdr lst))))
      (cond
       ((> n1 n2)
        ;;(pp (list "End at" n1 n2))
        0)
       (else
        ;; add n1 for sum, add 1 for counting
        (+ (if (is-twins? n1) (begin (dd n1) n1) 0)
           (loop (+ 1 n1) n2))))))))

(define (find-twins-1 lst)
  (find-twins ntwins? lst #t))

(define (find-twins-2 lst)
  (find-twins nntwins? lst #f))

(define (d1)
  (fold-add find-twins-1 nranges))

(define (d2)
  (fold-add find-twins-2 nranges))

;; 20223751480
(pp (d1))
;;(adv1 (d1))
;; 30260171216
(pp (d2))
;;(adv1 (list 1 2 3))
;;(adv2 (list 4 2 3))
