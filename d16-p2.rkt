#lang racket

;(define line "03036732577212944063491565474664")
(define line (first (port->lines (open-input-file "inputs/d16.txt"))))

; Signal/Pattern length
(define slen 0)
(define plen 4)

(define (parse-signal line)
  (set! slen (string-length line))
  (map (lambda (c) (string->number(string c))) (string->list line)))

(define signals (parse-signal line))
(define pattern (list 0 1 0 1))

(define signal-offset
  (string->number
   (string-trim (substring line 0 7) "0" #:repeat? #t)))

(define total-lines (- (* 10000 slen) signal-offset))

(define sv (make-vector total-lines 0))

(displayln (format "Signal offset ~a" signal-offset))
(displayln (format "Total lines ~a" total-lines))

; Load signals at offset into vector
(for-each
 (λ (idx)
   (let ([sidx (modulo (+ signal-offset idx) slen)])
     (vector-set! sv idx (list-ref signals sidx))))
 (range 0 total-lines))

(define (dump)
  (displayln (vector-copy sv 0 10))
  (displayln (vector-copy sv (- total-lines 10) total-lines)))

(define (ff)
  (foldl
   (λ (idx last)
     (let ([ret (modulo (+ last (vector-ref sv idx)) 10)])
       (vector-set! sv idx ret)
       ret))
   (vector-ref  sv (sub1 total-lines))
   (range (- total-lines 2) -1 -1)))

(dump)
(for ([n (range 0 100)])
  (displayln (format "Pharse ~a" n))
  (ff))
(dump)
