(use-modules (adv utils))
(use-modules (srfi srfi-1))

(define inputs (load-input "d5.txt"))

(define ranges
  (map (lambda(r)
         (let ((rr (string-split r #\-)))
           (map string->number rr)))
       inputs))

;;(pp ranges)

(define (list-split lst delim)
  (let loop ((ret '())
             (curr '())
             (lst lst))
    (cond
     ((null? lst) (append ret (list curr)))
     ((equal? (car lst) delim)
      (dd "Found delim")
      (loop (append ret (list curr))
            '()
            (cdr lst)))
     (else
      (dd (list "Check" (car lst)))
      (loop ret
            (append curr (list (car lst)))
            (cdr lst))))))

;;(pp (list-split ranges '(#f)))
(define ss (list-split ranges '(#f)))

(define (id-in-range range id)
  (and (>= id (car range))
       (<= id (cadr range))))

(define (find-fresh-ingr)
  (let* ((id-ranges (car ss))
         (ids (apply append (cadr ss))))
    (fold-add-parallel (lambda (id)
                (if (> (fold-add (lambda (range)
                            (if (id-in-range range id) 1 0))
                          id-ranges) 0)
                    1 0))
              ids)
    ))


(define (d1) (find-fresh-ingr))

;; 782
(pp (d1))

;; Overlap range can be merged
(define (can-merge? rn range)
  ;;(pp (list "can merge?" rn range))
  (let ((rn0 (car rn))
        (rn1 (cadr rn))
        (rg0 (car range))
        (rg1 (cadr range)))
    (or
     (and (>= rn1 rg0)
          (<= rn1 rg1))
     (and (>= rg0 rn0)
          (<= rg0 rn1))

     (and (>= rg1 rn0)
          (<= rg1 rn1))
     (and (>= rn0 rg0)
          (<= rn0 rg1))
     )))

(define (merge rn range)
  ;;(pp (list "merging" rn range))
  (let ((rn0 (car rn))
        (rn1 (cadr rn))
        (rg0 (car range))
        (rg1 (cadr range)))
    (list (min rn0 rg0) (max rn1 rg1))))

(define (merge-id-ranges)
  (let loop ((r1 (list (car (car ss))))
             (r2 (cdr (car ss)))
             (olen (length (car ss))))
    ;;(pp (list "loop1" r1 r2))
    (cond
     ((null? r2)
      ;; keep merging until no more mergeable
      (if (= (length r1) olen) r1
          (loop (list (car r1)) (cdr r1) (length r1))))
     (else
      (let ((range (car r2)))
        (let loop2 ((ret '())
                    (lst r1))
          ;;(pp (list "loop2" ret lst))
          (cond
           ((null? lst)
            ;; not merged till end, add to the end of list
            (loop (append ret (list range)) (cdr r2) olen))
           ((can-merge? (car lst) range)
            ;; merging
            (loop (append ret (list (merge (car lst) range)) (cdr lst))
                  (cdr r2) olen))
           (else
            ;; not merged, try next one
            (loop2 (append ret (list (car lst))) (cdr lst))))))
      ))))

;;(pp (merge-id-ranges))

(define (d2)
  (let ((merged-ranges (merge-id-ranges)))
    ;;(pp merged-ranges)
    (fold-add (lambda (range)
                (+ 1 (- (cadr range) (car range))))
              merged-ranges)))

;; 353863745078671
(pp (d2))
