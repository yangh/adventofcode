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

(tt (list "Uniq" (list-uniq '(3 4 5 3 4 5 1 1 1))))

(define START   #\S)
(define SPLITER #\^)
(define BEAM    #\|)
(define DOT     #\.)

(define start-pos (list-findp (list-ref matrix 0) START))

(dd inputs)
(dd (list "start pos" start-pos))

(define (beams-at row last-beams)
  (let ((fields (list-ref matrix row)))
    (list-uniq
     (fold-append
      (lambda (pos)
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
      last-beams))))

(define (beaming)
  ;; First beam at 2nd row
  (matrix-set! matrix 1 start-pos BEAM)

  (let loop ((row 2) (last-beams (list start-pos)))
    ;;(pp (list "beaming on" row (length last-beams)))
    (when (< row inputs-len)
      (loop (add1 row) (beams-at row last-beams)))))

(define (count-split-times)
  (beaming)
  (dd (map list->string matrix))

  (let ((row-len (length (list-ref matrix 0))))
    (fold-add-parallel
     (lambda (row)
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

;; Save sum on each node in a new matrix
(define matrix-nums
  (make-list matrix-len
             (make-list matrix-row-len 0)))

(define (matrix-sum-up row pos)
  (if (= row 1)
      (if (= pos start-pos) 1 0)
      (let ((row-up (sub1 row)))
        (+
         ;; up
         (matrix-ref matrix-nums row-up pos)
         ;; left
         (if (and (> pos 0)
                  (char=? SPLITER (matrix-ref matrix row (sub1 pos))))
             (matrix-ref matrix-nums row-up (sub1 pos)) 0)
         ;; right
         (if (and (< pos (sub1 matrix-row-len))
                  (char=? SPLITER (matrix-ref matrix row (add1 pos))))
             (matrix-ref matrix-nums row-up (add1 pos)) 0)
         ))))

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
