#lang racket

(require math/base)
(require "utils.rkt")

(define lines (input-load-lines "6-0"))

(define mrows (length lines))
(define mcols (string-length (first lines)))

(define matrix (make-vector (* mrows mcols) #\.))

(define OBSTR #\#)
(define GUARD #\^)
(define PATHX #\X)

(struct pos (x y) #:mutable #:transparent)

(define guard-dir-offsets (list (pos  1  0)  ;>
                                (pos  0  1)  ;v
                                (pos -1  0)  ;<
                                (pos  0 -1)));^
(define dir-chars (string->list ">v<^"))
(define (dir->char d) (list-ref dir-chars d))
(define (char->dir c) (index-of dir-chars c))
(define (dir->guard-offset d) (list-ref guard-dir-offsets d))
(define (dir-turn-right d)
  (if (< d 3) (+ d 1) 0))

(define guard (pos 0 0))
(define guard-dir 0)

(define (pos-to-idx x y) (+ x (* mcols y)))
(define (pos-add p1 p2) (pos (+ (pos-x p1) (pos-x p2))
                             (+ (pos-y p1) (pos-y p2))))
(define (pos-valid? pos)
  (let ([x (pos-x pos)]
        [y (pos-y pos)])
    (and (>= x 0)
         (>= y 0)
         (< x mcols)
         (< y mrows))))

(define (pos-eq? p1 p2) (and (= (pos-x p1) (pos-x p2))
                             (= (pos-y p1) (pos-y p2))))

(define (matrix-get pos)
  (vector-ref matrix (pos-to-idx (pos-x pos) (pos-y pos))))
(define (matrix-set! pos c)
  (vector-set! matrix (pos-to-idx (pos-x pos) (pos-y pos)) c))
(define (matrix-dump)
  (for ([col (range mcols)])
    (for ([row (range mrows)])
      (display (matrix-get (pos row col))))
    (displayln "")))

(for* ([col (range mcols)]
       [row (range mrows)])
  (let ([c (string-ref (list-ref lines col) row)])
    ;(displayln c)
    (cond
      ([eq? c GUARD] (set-pos-x! guard row)
                     (set-pos-y! guard col)
                     (set! guard-dir (char->dir c)))
      ([eq? c OBSTR] (matrix-set! (pos row col) OBSTR)))))

;(displayln guard-dir)
;(displayln (dir->char guard-dir))

(define last-guard guard)

(let loop ()
  (let* ([offset (dir->guard-offset guard-dir)]
         [next-p (pos-add guard offset)])
    ;(matrix-dump)
    ;(sleep 1)
    ;(displayln (format "~a -> ~a" guard next-p))
    (when (pos-eq? last-guard next-p)
      (displayln (format "Found loop ~a" last-guard)))
    (cond
      ([not (pos-valid? next-p)]      ; out of map
       (displayln (format "Out of map ~a" next-p)))
      (else
       (let ([next-c (matrix-get next-p)])
         (cond
           ([eq? next-c OBSTR]        ; turn right
            (set! guard-dir (dir-turn-right guard-dir))
            (loop))
           (else                      ; move to next pos
            (matrix-set! guard PATHX)
            (set! guard next-p)
            (matrix-set! guard (dir->char guard-dir))
            (loop))))))))

;(matrix-dump)

; 4751
(count (lambda (c) (or (eq? c PATHX)
                       (eq? c GUARD)))
       (vector->list matrix))
