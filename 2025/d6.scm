(use-modules (adv utils))
(use-modules (srfi srfi-1))

(define inputs (load-input "d6.txt"))

(define len (length inputs))

(define (char->op ch)
  (cond
   ((char=? ch #\*) *)
   ((char=? ch #\+) +)))

(define ops (map char->op
                 (map (lambda (s) (string-ref s 0))
                      (string-tokenize (list-ref inputs (- len 1))))))

(dd inputs)
(dd ops)

;; Convert string to numbers
(define numbers
  (map (lambda (r)
         (let ((rr (string-tokenize r)))
           (map string->number rr)))
       inputs))

;; Replace ops line with nops, a + 0 = a, a * 1 = a
(list-set! numbers (- len 1)
           (map (lambda (op)
                  (if (eqv? op +) 0 1)) ops))
;;(pp numbers)

(define (d1)
  (apply + (map (lambda (op id)
                  (apply op (map (lambda (row)
                                   (list-ref (list-ref numbers row) id))
                                 (iota (length numbers)))))
                ops (iota (length ops)))
         ))

;; 5322004718681
(pp (d1))

(define char-0-int (char->integer #\0))

(define (char->digit ch) (- (char->integer ch) char-0-int))

(define char-digit-space (char->digit #\space))

(define (filter-char ch lst)
  (filter (lambda (c) (not (char=? c ch))) lst))

(define (filter-num num lst)
  (filter (lambda (n) (not (= n num))) lst))

(define matrix (map (lambda (s) (string->list s)) inputs))
(define matrix-num (map (lambda (s)
                          (map char->digit (string->list s)))
                        inputs))
(dd matrix)
(dd matrix-num)

(define (num-at-col col)
  (let* ((nums (map (lambda (row) (list-ref (list-ref matrix-num row) col))
                    (iota (- (length matrix) 1))))
         (nums-filtered (filter-num char-digit-space nums)))
    ;;(pp nums)
    nums-filtered
    ))

;; Convert num list to num
(define (list->num lst)
  (if (null? lst) #f
      (let loop ((n 0) (lst lst))
        (if (null? lst) n
            (loop (+ (* 10 n) (car lst))
                  (cdr lst))
            ))))

(define nums-ll (map num-at-col (iota (length (list-ref matrix 0)))))
;;(list-set! nums-ll (- (length nums-ll) 1) '())
(dd nums-ll)
(define nums-list (map list->num nums-ll))
(dd nums-list)

(define ops-list (list-ref matrix (- (length matrix) 1)))

(define (d2)
  (let loop ((sum 0)
             (values '())
             (op-idx (- (length ops-list) 1)))
    (if (< op-idx 0)
        sum
        (let ((op  (list-ref ops-list op-idx))
              (num (list-ref nums-list op-idx)))
          (cond
           ((char=? op #\space)
            (if (integer? num)
                (loop sum (append values (list num)) (- op-idx 1))
                (loop sum values (- op-idx 1))
                ))
           ((char=? op #\+)
            (loop (+ sum (apply + (append values (list num)))) '() (- op-idx 1)))
           ((char=? op #\*)
            (loop (+ sum (apply * (append values (list num)))) '() (- op-idx 1)))
     )))))

;; 9876636978528
(pp (d2))
