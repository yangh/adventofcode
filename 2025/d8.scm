(use-modules (adv utils))
(use-modules (srfi srfi-1))

(define inearest-puts (load-input "d8"))

(define len (length inearest-puts))

;; Convert string to numbers
(define numbers
  (map (lambda (r)
         (let ((rr (string-split r #\,)))
           (map string->number rr)))
       inearest-puts))

(dd numbers)

(define (square n) (* n n))

(define (distance-3d p1 p2)
  (let (
        (x0 (list-ref p1 0))
        (y0 (list-ref p1 1))
        (z0 (list-ref p1 2))
        (x1 (list-ref p2 0))
        (y1 (list-ref p2 1))
        (z1 (list-ref p2 2))
        )
    (sqrt (+
           (square (- x1 x0))
           (square (- y1 y0))
           (square (- z1 z0))
           ))
    ))

; return (list p1 p2 dist remaind-points-alist)
(define (find-nearest p lst)
  (let loop ((ret '())
             (nearest-p (car lst))
             (np-dist (distance-3d p (car lst)))
             (lst (cdr lst)))
    (if (null? lst)
        (list p nearest-p np-dist ret)
        (let* ((tmpp (car lst))
               (tmpp-dist (distance-3d p tmpp)))
          ;;(dd (list "distance" tmpp-dist p tmpp))
          (if (< tmpp-dist np-dist)
              ;;(loop (cons (list nearest-p np-dist) ret) tmpp tmpp-dist (cdr lst))
              ;;(loop (cons (list tmpp tmpp-dist) ret) nearest-p np-dist (cdr lst)))
              (loop (acons nearest-p np-dist ret) tmpp tmpp-dist (cdr lst))
              (loop (acons tmpp tmpp-dist ret) nearest-p np-dist (cdr lst)))
              ))))

(define (assoc-keys alist)
  (map car alist))

(define (assoc-value-mini alist)
  (let loop ((amini (car alist))
             (alist (cdr alist)))
    (if (null? alist) amini
        (let ((anext (car alist)))
          ;;(dd (list amini anext))
          (if (< (cdr amini) (cdr anext))
              (loop amini (cdr alist))
              (loop anext (cdr alist))
              ))
        )))

(define (circute p1 p2 dist)
  (dd "Build circute" p1 p2 dist)
  (cons (list p1 p2)
        (acons (cons p1 p2) dist '())))

;; Add p into circute at head or tail
(define (circute-add ct p)
  (let* ((plist (car ct))
         (p1 (car plist))
         (p2 (list-ref plist (sub1 (length plist))))
         (d1 (distance-3d p1 p))
         (d2 (distance-3d p2 p))
         )
    (cons
     (if (< d1 d2)
         (cons p plist)
         (append plist (list p)))
     (assoc-set! (cdr ct)
                 (cons p (if (< d1 d2) p1 p2))
                 (if (< d1 d2) d1 d2)))
    ))

(define (circute-head ct)
  (car (car ct)))

(define (circute-tail ct)
  (let ((plist (car ct)))
    (list-ref plist (sub1 (length plist)))))

(define circutes '())
(define boxes (list-copy numbers))

(define (try-find)
  (let* ((np1 (find-nearest (car numbers) (cdr numbers)))
         (np2 (find-nearest (list-ref np1 1) (assoc-keys (list-ref np1 3))))
         (p3 (assoc-value-mini (list-ref np1 3)))
         )
    (dd np1 np2 p3)
    (set! circutes
          (list
           (circute-add
            (circute
             (list-ref np1 0)
             (list-ref np1 1)
             (list-ref np1 2))
            (list-ref np2 1))))
    (set! boxes (assoc-keys (list-ref np2 3)))
    ))

;;(try-find)
;;(pp circutes)
;;(dd boxes)

(define (circutes-leafs)
  (fold-append (lambda (ct)
                 (list
                  (cons (circute-head ct) ct)
                  (cons (circute-tail ct) ct)
                  ))
               circutes
               ))

(define (filter-p p lst)
  (filter (lambda (n) (not (equal? n p))) lst))

(define (numbers-filter numbers lst)
  (let loop ((lst1 numbers)
             (lst2 lst))
    (if (null? lst2) lst1
        (loop (filter-p (car lst2) lst1) (cdr lst2)))))

;;(dd (circutes-leafs))
(define (circute-add! ct p)
  (dd "Add p to cr" ct p)
  (if (null? circutes) (list ct)
      (let loop ((heads '())
                 (tails circutes))
        (cond
         ((null? tails)
          (pp "ERROR: NOT FOUND!")
          circutes)
          ((equal? ct (car tails))
           (append heads
                   (list (circute-add (car tails) p))
                   (cdr tails)))
          (else
           (loop (append heads (list (car tails)))
                 (cdr tails))))
         )
      )
  )

(define (next-find)
  (let loop ((i 0))
    (let* ((cr-leafs (circutes-leafs))
           (numbers (append boxes (assoc-keys cr-leafs)))
           (np1 (find-nearest (car numbers) (cdr numbers))))
      ;;(dd numbers np1)
      ;;(dd np1)
      (let ((ct  (assoc-ref cr-leafs (list-ref np1 1))))
        (set! circutes
              (if ct
                  ;; Add to exist circute
                  (circute-add! ct (list-ref np1 0))
                  ;; Create new circute
                  (append circutes
                          (list (circute
                                 (list-ref np1 0)
                                 (list-ref np1 1)
                                 (list-ref np1 2))
                                )))))
      (set! boxes (numbers-filter
                   (assoc-keys (list-ref np1 3))
                   (assoc-keys cr-leafs))))
    (dd i circutes boxes)
    (when (and (< i 10)
               (not (null? boxes)))
      (loop (add1 i)))
    ))

(dd (next-find))
