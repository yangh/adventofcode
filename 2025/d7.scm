(use-modules (adv utils))
(use-modules (srfi srfi-1))

(define inputs (load-input "d7"))

(define inputs-len (length inputs))
(define inputs-row-len (string-length (list-ref inputs 0)))

(define matrix (map (lambda (s) (string->list s)) inputs))
(define matrix-len (length matrix))
(define matrix-row-len (length (list-ref matrix 0)))

(define (matrix-ref lst row col)
  (list-ref (list-ref lst row) col))

(define (matrix-set! lst row col val)
  (list-set! (list-ref lst row) col val))

(define (list-findp lst v)
  (let find ((p 0) (lst lst))
    (cond
     ((null? lst) #f)
     ((eq? (car lst) v) p)
     (else (find (add1 p) (cdr lst))))))

(define (list-add-uniq lst v)
  (if (list-findp lst v)
      lst
      (append lst (list v))))

(define (list-uniq lst)
  (let loop ((ret '()) (lst lst))
    ;;(pp (list "list-uniq" ret lst))
    (if (null? lst) ret
        (loop (list-add-uniq ret (car lst))
              (cdr lst)))))

(tt (list "Uniq" (list-uniq '(3 4 5 3 4 5 1 1 1))))

(define START   #\S)
(define SPLITER #\^)
(define BEAM    #\|)
(define DOT     #\.)

(define start-pos (list-findp (list-ref matrix 0) START))

(dd inputs)
(dd (list "start pos" start-pos))

(define (beaming)
  ;; First beam at 2nd row
  (matrix-set! matrix 1 start-pos BEAM)

  (let loop ((row 2) (last-beams (list start-pos)))
    ;;(pp (list "beaming on" row (length last-beams)))
    (when (< row inputs-len)
      (let ((fields (list-ref matrix row)))
        (loop (add1 row)
              (let ((new-beams
                     (list-uniq
                      (apply append
                             (map (lambda (pos)
                                    (let ((f (list-ref fields pos)))
                                      (cond
                                       ((char=? f SPLITER)
                                        ;;(dd (list "spliter" row pos))
                                        (matrix-set! matrix row (sub1 pos) BEAM)
                                        (matrix-set! matrix row (add1 pos) BEAM)
                                        (list (sub1 pos) (add1 pos)))
                                       (else
                                        (matrix-set! matrix row pos BEAM)
                                        (list pos)))))
                                  last-beams)))))
                ;;(pp new-beams)
                new-beams)))
      #f)))

(define (count-split-times)
  (beaming)
  (dd (map list->string matrix))

  (let ((row-len (length (list-ref matrix 0))))
    (fold-add-parallel (lambda (row)
                (if (< row 2) 0
                    (let ((fields-prev (list-ref matrix (sub1 row)))
                          (fields (list-ref matrix row)))
                      (fold-add (lambda (col)
                                  (if (and
                                       (char=? (list-ref fields col) SPLITER)
                                       (char=? (list-ref fields-prev col) BEAM)
                                       )
                                      1 0))
                                (iota row-len)))))
              (iota inputs-len))))

(define matrix-nums
  (make-list matrix-len (make-list matrix-row-len 0)))

(define (matrix-sum-up row pos)
  (if (= row 1)
      (if (= pos start-pos) 1 0)
      (+
       (matrix-ref matrix-nums (sub1 row) pos)
       (if (not (and (> pos 0)
                     (char=? SPLITER (matrix-ref matrix row (sub1 pos)))))
           0
           (matrix-ref matrix-nums (sub1 row) (sub1 pos)))
       (if (not (and (< pos (sub1 matrix-row-len))
                     (char=? SPLITER (matrix-ref matrix row (add1 pos)))))
           0
           (matrix-ref matrix-nums (sub1 row) (add1 pos)))
       )))

(define (matrix-sum row)
  (when (> row 0)
    (list-set! matrix-nums row
               (map (lambda (pos)
                      (if (char=? BEAM (matrix-ref matrix row pos))
                          (matrix-sum-up row pos)
                          0))
                    (iota matrix-row-len)
                    ))))

;; top to bottom, simple works, but 140 lines too complex to resolve
(define (count-particle-path)
  (map matrix-sum (iota matrix-len))
  (dd matrix-nums)
  (apply + (list-ref matrix-nums (sub1 matrix-len))))

(define (d1) (count-split-times))

(pp (d1))

;; Depends on d1
(define (d2) (count-particle-path))

;; 34339203133559
(pp (d2))
