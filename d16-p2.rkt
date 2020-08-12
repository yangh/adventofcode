#lang racket

;Day16: Part 2
;  
; Accumlate sum the triangle start at the signal offset
; at the right-bottom corner.
;    
;  1 1 1 1
;  0 1 1 1  < signal offset
;  0 0 1 1
;  0 0 0 1

;(define line "03036732577212944063491565474664")
(define line (first (port->lines (open-input-file "inputs/d16.txt"))))

(define pattern (list 0 1 0 1))
(define signals #f)
(define signal-offset 0)
(define total-lines 0)
(define sv #f)

; Signal/Pattern length
(define slen 0)
(define plen (length pattern))

(define (parse-signal line)
  (set! slen (string-length line))
  (set! signals (map (lambda (c)
                       (string->number(string c)))
                     (string->list line)))
  (set! signal-offset
        (string->number
         (string-trim (substring line 0 7) "0" #:repeat? #t)))
  (set! total-lines (- (* 10000 slen) signal-offset))

  (displayln (format "Signal offset: ~a" signal-offset))
  (displayln (format "Total lines:   ~a" total-lines))

  (set! sv (make-vector total-lines 0))
  ; Load signals at offset into vector
  (for-each
   (λ (idx)
     (let ([sidx (modulo (+ signal-offset idx) slen)])
       (vector-set! sv idx (list-ref signals sidx))))
   (range 0 total-lines)))

(define (dump)
  (displayln (format "Head: ~a" (vector-copy sv 0 8)))
  (displayln (format "Tail: ~a"
                     (vector-copy sv (- total-lines 8) total-lines))))

(define (fft)
  (foldl
   (λ (idx last)
     (let ([ret (modulo (+ last (vector-ref sv idx)) 10)])
       (vector-set! sv idx ret)
       ret))
   (vector-ref sv (sub1 total-lines)) ; Start from the last signal
   (range (- total-lines 2) -1 -1)))  ; Backward accumlate translate

(define (xrun proc n msg)
  (display msg)
  (for ([n (range 0 n)])
    (display ".")
    (flush-output)
    (proc))
  (displayln ""))

(parse-signal line)
(dump)
(xrun fft 100 "Pharse: ")
(dump)
