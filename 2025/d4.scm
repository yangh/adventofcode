(use-modules (adv utils))
(use-modules (srfi srfi-1))

(define inputs (map string->list (load-input "d4.txt")))

(define rows (length inputs))
(define cols (length (car inputs)))
(define wrap-offset 1)

;;(pp (list rows cols))

(define ROLL #\@)
(define SPCS #\.)

;; Offset of easter/south/west/north
(define matrix-eswn
  '(     (0 1)
    (-1 0)   (1 0)
        (0 -1)
    ))

;; Offset of cross pos of eswn
(define matrix-eswn-x
  '((-1  1) (1  1)
    (-1 -1) (1 -1)))

;; All 8 pos around
(define matrix-eswn-8
  (append matrix-eswn matrix-eswn-x))
;;(pp matrix-eswn-8)

(define (map-eswn-8 x y offset)
  (map (lambda (mx) (list (+ x offset (car mx))
                          (+ y offset (cadr mx))))
       matrix-eswn-8))

;; Indirect refer
(define (char-at-wrapped x y)
  (list-ref (list-ref inputs (+ wrap-offset y)) (+ wrap-offset x)))

;; Direct refer
(define (char-at x y)
  (list-ref (list-ref inputs y) x))

(define (count-rolls row col)
  (dd (list "count for" row col))
  (fold-add (lambda (pos)
              (dd (list "check matrix" pos))
              (let* ((x (car pos))
                     (y (cadr pos))
                     (ch (char-at x y)))
                (if (char=? ch ROLL) 1 0)))
            (map-eswn-8 row col wrap-offset)))

;; Add some 'space bar' around the original data
;; which help us to avoid checking the bounder.
;; the default width of the bar is 1.
(define (wrap-inputs inputs ch)
  (let* ((rows (length inputs))
         (cols (length (car inputs)))
         (cols-new (+ (* 2 wrap-offset) cols)))
    (dd (list "wrap" rows cols cols-new wrap-offset))
    (append (list (make-list cols-new ch))
            (map (lambda(row)
                   (append (list ch) row (list ch)))
                 inputs)
            (list (make-list cols-new ch))
            )))

;; Find all roll on the row, return the count
(define (find-roll row)
  (dd (list "find row" row))
  (fold-add (lambda (col)
              (dd (list "find col" row))
              (if (and (char=? ROLL (char-at-wrapped row col))
                       (< (count-rolls row col) 4))
                  1 0))
            (iota cols)))

(define (d1)
  (fold-add-parallel find-roll (iota rows)))

;; Wrap inputs to avoid checking bounder
(set! inputs (wrap-inputs inputs SPCS))
;;(pp (map list->string inputs))

;; 1351
(pp (d1))

(define (find-roll-pos row)
  ;;(dd (list "find row" row))
  (apply append (map (lambda (col)
                 ;;(dd (list "find col" row))
                 (if (and (char=? ROLL (char-at-wrapped row col))
                          (< (count-rolls row col) 4))
                     (list (list row col)) '()))
               (iota cols))))

(define (collect-roll)
  (fold-append-parallel find-roll-pos (iota rows)))

(define (char-set-wrapped pos ch)
  (list-set! (list-ref inputs (+ wrap-offset (cadr pos)))
             (+ wrap-offset (car pos)) ch))

(define (clean-roll pos)
  (char-set-wrapped pos SPCS))

(define (clean-rolls rolls)
  (map clean-roll rolls))

(define (find-all-roll)
  (let loop ((rolls (collect-roll))
             (count 0))
    (if (= (length rolls) 0)
        count
        (begin (clean-rolls rolls)
               (loop (collect-roll) (+ count (length rolls)))))))

(define (d2) (find-all-roll))

;; 8345
(pp (d2))
