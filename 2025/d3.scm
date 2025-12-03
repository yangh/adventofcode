(use-modules (adv utils))
(use-modules (srfi srfi-1))

(define inputs (load-input "d3.txt"))

(define (sub1 n) (- n 1))
(define (sub2 n) (- n 2))
(define (add1 n) (+ n 1))
(define char-0-int (char->integer #\0))

(define (char->digit ch)
  (- (char->integer ch) char-0-int))

(define (batt-lists lines)
  (map (lambda(line) (map char->digit (string->list line))) lines))

(define (p-0 mp) (car (car mp)))
(define (p-1 mp) (car (cadr mp)))
(define (pos-0 mp) (cadr (car mp)))
(define (pos-1 mp) (cadr (cadr mp)))

;; find the top 2 numbers in the list
(define (find-max-pair batts)
  (dd (list "find in" (list->string (map (lambda(n) (integer->char (+ n char-0-int))) batts))))
  (let loop ((batts batts)
             (pos (length batts))
             (mpair '((0 0) (0 0))))
    (cond
     ((null? batts) mpair)
     (else
      (let ((n (car batts)))
        (cond
         ((and (> n (p-1 mpair)) (> n (p-0 mpair)))
          (loop (cdr batts) (sub1 pos) (list (list n pos) (cadr mpair))))
         ((> n (p-1 mpair)) (loop (cdr batts) (sub1 pos) (list (car mpair) (list n pos))))
         (else (loop (cdr batts) (sub1 pos) mpair)))))
     )))

;;(tt (find-max-pair '(2 1 5 9 7 1 8)))
;;(tt (find-max-pair '(1 1 1 1)))

(define (find-max-in-range nlist start end)
  (dd (list "find in range"
            (list->string
             (map (lambda(n) (integer->char (+ n char-0-int))) nlist))
            start end))
  (let loop ((pos start) (max '(0 0)))
      (cond
       ((> pos end) max)
       ((> (list-ref nlist pos) (car max))
        (loop (add1 pos) (list (list-ref nlist pos) pos)))
       (else
        (loop (add1 pos) max)
        ))))

;;(pp (find-max-in-range '(2 1 5 9 7 1 8) 0 3))
;;(pp (find-max-in-range '(2 1 5 9 7 1 8) 0 2))

;; max num pair consist by:
;; p1: max in position 0~ (len - 2)
;; p2: max in position (pos p1) (len - 1)
(define (find-max-pair-r2 batts)
  (let* ((len (length batts))
         (p1 (find-max-in-range batts 0 (sub2 len)))
         (p2 (find-max-in-range batts (add1 (cadr p1)) (sub1 len))))
    (dd (list p1 p2))
    (+ (* 10 (car p1))
       (car p2))))

;; search range:
;; start - position of last found max num
;; end - (- len (remined nums))
;; 12345678901234567980
;;   3 |->         <-|
;;      slid window
(define (find-max-pair-rn batts rn)
  (let ((len (length batts)))
    (let loop ((start 0)
               (rn rn))
      (if (= rn 0) 0
          (let ((max (find-max-in-range batts start (- len rn))))
            (dd (list "found" max))
            (+ (* (expt 10 (sub1 rn)) (car max))
               (loop (add1 (cadr max)) (sub1 rn))))
          ))))

(define (d1 inputs)
  (fold-add find-max-pair-r2 (batt-lists inputs)))

(define (d2 inputs)
  (fold-add (lambda (lst) (find-max-pair-rn lst 12))
            (batt-lists inputs)))

(define (d1-v2 inputs)
  (fold-add (lambda (lst) (find-max-pair-rn lst 2))
            (batt-lists inputs)))

(define simples (list
                 "987654321111111"
                 "811111111111119"
                 "234234234234278"
                 "818181911112111"))
;; 357
;;(pp (d1 simples))
;; 17113
;;(pp (d1 inputs))
(pp (d1-v2 inputs))

;; 3121910778619
;;(pp (d2 simples))
;; 169709990062889
;;(pp (d2 inputs))
