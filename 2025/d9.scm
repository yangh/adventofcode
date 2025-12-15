(use-modules (adv utils))
(use-modules (srfi srfi-1))

(define inearest-puts (load-input "d9"))

(define len (length inearest-puts))

;; Convert string to numbers
(define numbers
  (map (lambda (r)
         (let ((rr (string-split r #\,)))
           (map string->number rr)))
       inearest-puts))

;;(dd numbers)

(define (pos-x<? p1 p2)
  (let ((x0 (list-ref p1 0))
        (x1 (list-ref p2 0))
        (y0 (list-ref p1 1))
        (y1 (list-ref p2 1)))
    (if (= x0 x1)
        (< y0 y1)
        (< x0 x1))
    ))

(define (pos-x>? p1 p2)
  (let ((x0 (list-ref p1 0))
        (x1 (list-ref p2 0))
        (y0 (list-ref p1 1))
        (y1 (list-ref p2 1)))
    (if (= x0 x1)
        (> y0 y1)
        (> x0 x1))
    ))

(define (pos-y<? p1 p2)
  (let ((x0 (list-ref p1 0))
        (x1 (list-ref p2 0))
        (y0 (list-ref p1 1))
        (y1 (list-ref p2 1)))
  (if (= y0 y1)
      (< x0 x1)
      (< y0 y1))
  ))

(define (pos<? p1 p2)
  (let ((x0 (list-ref p1 0))
        (x1 (list-ref p2 0))
        (y0 (list-ref p1 1))
        (y1 (list-ref p2 1)))
    (or (< x0 x1)
        (< y0 y1))))

(define (square n) (* n n))

(define (pos-dist>? p1 p2)
  (let ((x0 (list-ref p1 0))
        (y0 (list-ref p1 1))
        (x1 (list-ref p2 0))
        (y1 (list-ref p2 1)))
    (or (>
         ;; distance against (0, 0)
         (sqrt (+ (square x0) (square y0)))
         (sqrt (+ (square x1) (square y1)))
         ))))

(define (surface-size p1 p2)
  (let ((x0 (list-ref p1 0))
        (y0 (list-ref p1 1))
        (x1 (list-ref p2 0))
        (y1 (list-ref p2 1)))
    (abs (* (- x1 x0 -1)
            (- y1 y0 -1)))
    ))

(define numbers-sorted (sort numbers pos<?))
;;(dd numbers-sorted)

;;(dd (sort numbers pos-dist>?))

;;(dd (sort numbers pos<?))
;;(dd (sort numbers pos-x<?))
;;(dd (sort numbers pos-x>?))

(define (find-max-sf lst valid-rect?)
  (let loop ((maxsf 0)
             (maxp1p2 #f)
             (p1 (car lst))
             (lst (cdr lst))
             (lst-pending (cdr lst)))
    (if (null? lst)
        (if (null? lst-pending)
            (cons maxsf maxp1p2)
            (begin
              ;;(dd maxsf maxp1p2 lst lst-pending)
              (loop maxsf maxp1p2
                    (car lst-pending)
                    (cdr lst-pending)
                    (cdr lst-pending))
              ))
        (let* ((p2 (car lst))
               (sf (surface-size p1 p2)))
          ;;(dd p1 p2 sf)
          (if (and (> sf maxsf)
                   (valid-rect? p1 p2))
              (loop sf (list p1 p2) p1 (cdr lst) lst-pending)
              (loop maxsf maxp1p2 p1 (cdr lst) lst-pending))
          ))
    ))

;; 4733727792
(dd (find-max-sf numbers-sorted
                 (lambda (p1 p2) #t)))

(define num-hash (make-hash-table 128))
(define num-green-hash (make-hash-table 128))
(define num-green-hash-hit 0)

(define (num-hash-id-x p1) (* 100000000 (list-ref p1 0)))
(define (num-hash-id-y p1) (list-ref p1 1))

(define (num-hash-id p1)
  (+ (num-hash-id-x p1)
     (num-hash-id-y p1)))

(define (num-hash-ref p1)
  (hash-ref num-hash (num-hash-id p1)))

(define (num-hash-xy-ref id)
  (hash-ref num-hash id))

(define (range-extend p n)
  (let* ((x (list-ref p 0))
         (y (list-ref p 1)))
    (cond
     ((< n x) (list n y))
     ((> n y) (list x n))
     (else (list x y)))
    ))

(define (range-has? p n)
  (let* ((x (list-ref p 0))
         (y (list-ref p 1)))
    (and (>= n x)
         (<= n y))
    ))

(define (pos-on-the-green-line? p findx? findy?)
  (let* ((x (list-ref p 0))
         (y (list-ref p 1))
         (xid (num-hash-id-x p))
         (yid (num-hash-id-y p))
         (yrange (num-hash-xy-ref xid))
         (xrange (num-hash-xy-ref yid))
         )
    ;; TBD: add cache
    (let ((online (or
                   (and findy? xrange (range-has? xrange x))
                   (and findx? yrange (range-has? yrange y))
                   )))
      ;;(when online (dd p xrange yrange))
      online
      )))

;; Offset of easter/south/west/north
(define matrix-eswn
  '(     (0 1)
         (-1 0)   (1 0)
         (0 -1)
         ))

(define global-xrange
  (let* ((x-sorted (sort numbers pos-x<?)))
    (list
     (car (list-head1 x-sorted))
     (car (list-tail1 x-sorted)))
    ))

(define global-yrange
  (let* ((y-sorted (sort numbers pos-y<?)))
    (list
     (cadr (list-head1 y-sorted))
     (cadr (list-tail1 y-sorted)))
    ))

(dd global-xrange global-yrange)

(define (find-x-line-on-dir x y dir xrange)
  (let loop ((x x))
    (if (range-has? xrange x)
        (if (pos-on-the-green-line? (list x y) #t #f) #t
            (loop (dir x)))
        #f)))

(define (find-x-line x y)
  (and
   (find-x-line-on-dir x y sub1 (list (car global-xrange) x))
   (find-x-line-on-dir x y add1 (list x (cadr global-xrange)))
   ))

(define (find-y-line-on-dir x y dir yrange)
  (let loop ((y y))
    (if (range-has? yrange y)
        (if (pos-on-the-green-line? (list x y) #f #t) #t
            (loop (dir y)))
        #f)))

(define (find-y-line x y)
  (and
   (find-y-line-on-dir x y sub1 (list (car global-yrange) y))
   (find-y-line-on-dir x y add1 (list y (cadr global-yrange)))
   ))

(use-modules (ice-9 threads))
(use-modules (ice-9 futures))

(define (pos-in-the-green-square? p)
  ;;(dd "pINgsqrt?" p)
  (let* ((x (list-ref p 0))
         (y (list-ref p 1))
         (nid (num-hash-id p))
         (green-cached (hash-ref num-green-hash nid)))
    (let ((found?
           (or (and green-cached (cadr green-cached))
               (every identity (par-map (lambda (proc)
                                          (proc x y))
                                        (list find-x-line
                                              find-y-line)))
               )))
      ;;(pp (list "Find green" p green-cached found?))
      (if (not green-cached)
            (hash-set! num-green-hash nid (list 1 found?))
            (hash-set! num-green-hash nid (list (add1 (car green-cached))
                                                (cadr green-cached))))
      found?)
    ))

(define (pos-is-valid? x y)
  (or
   ;; Known point
   (num-hash-ref (list x y))
   ;; Point in the middle of line (green)
   ;;(pos-on-the-green-line? (list x y) #t #t)
   ;; Point in the green square?
   (pos-in-the-green-square? (list x y))
   ))

(define (num-hash-xy-set! p)
  (let* ((x (list-ref p 0))
         (y (list-ref p 1))
         (xid (num-hash-id-x p))
         (yid (num-hash-id-y p))
         (yrange (num-hash-xy-ref xid))
         (xrange (num-hash-xy-ref yid))
         )
    (if yrange
        (hashq-set! num-hash xid (range-extend yrange y))
        (hashq-set! num-hash xid (list y y)))
    (if xrange
        (hashq-set! num-hash yid (range-extend xrange x))
        (hashq-set! num-hash yid (list x x)))
    ))

(define (rect-red-and-green? p1 p2)
  (let ((x0 (list-ref p1 0))
        (x1 (list-ref p2 0))
        (y0 (list-ref p1 1))
        (y1 (list-ref p2 1)))
    (or
     ;; Single line, useless
     (= x0 x1)
     (= y0 y1)
     (and
      ;; The other corners is valid?
      (pos-is-valid? x0 y1)
      (pos-is-valid? x1 y0))
     )))

(for-each (lambda (p)
            ;; x...y
            (hashq-set! num-hash (num-hash-id p) p)
            ;; x....
            ;; ....y
            (num-hash-xy-set! p)
            )
          numbers)

(dd (hash-map->list cons num-hash))
;;(dd (num-hash-ref '(11 1)))

;; 93042, too low
;; 4586713546, too high 1m36s solo
;;(dd (find-max-sf numbers-sorted rect-red-and-green?))

;;(dd (hash-map->list cons num-green-hash))

(define (find-edges)
  (let loop ((x (car global-xrange))
              (lst '()))
    (newline)
    (pp (list x lst))
    (let* ((xid (num-hash-id-x (list x 0)))
           (yrange (num-hash-xy-ref xid))
           (llen (length lst)))
      (when (not yrange)
        (pp (list "Not Found" xid)))
      (pp (list "yrange" yrange "x" x))
      (if (or
           (= (length lst) (length numbers))
           ;;(null? lst)
           )
          lst
          (let* (
                 (last-y
                  (if (null? lst)
                      (cadr yrange)
                      (let* ((ly1 (cadr (list-ref lst (- llen 2))))
                             (ly2 (cadr (list-ref lst (- llen 1))))
                             (ny1 (car yrange))
                             (ny2 (cadr yrange))
                             )
                        (if (or (= ny1 ly1)
                                (= ny1 ly2))
                            ny2
                            ny1)
                        )))
                 (yid (num-hash-id-y
                       (list 0 last-y)))
                 (xrange (num-hash-xy-ref yid))
                 )
            (when (not xrange)
              (pp (list "Not Found" xid)))
            (pp (list "xrange" xrange "last-y" last-y))
            (loop
             (if (= last-y (car yrange))
                 (car xrange)
                 (cadr xrange)
                 )
             (append lst
                     (if (null? lst)
                         (list
                          (list x (car yrange))
                          (list x (cadr yrange)))
                         (let* ((ly1 (cadr (list-ref lst (- llen 2))))
                                (ly2 (cadr (list-ref lst (- llen 1))))
                                (ny1 (car yrange))
                                (ny2 (cadr yrange))
                                )
                           (if (= ly2 ny1)
                               (list
                                (list x (car yrange))
                                (list x (cadr yrange)))
                               (list
                                (list x (cadr yrange))
                                (list x (car yrange)))
                               )))
                     ))
            )))
    ))

(dd (find-edges))
