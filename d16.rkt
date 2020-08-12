#lang racket

(define line "12345678")

; Signal/Pattern length
(define slen 0)
(define plen 4)

(define (parse-signal line)
  (set! slen (string-length line))
  (map (lambda (c) (string->number(string c))) (string->list line)))

(define signals (parse-signal line))

(define pattern (list 0 1 0 -1))

(define (gen-pattern n)
  (foldl (lambda ( v result)
           (append result (make-list n v)))
         '() pattern))

(define (sump sigs pat lno)
  (foldl (lambda (idx result)
           (let ([n (list-ref sigs idx)]
                 [p (list-ref pat (modulo (add1 idx) plen))])
             ;(displayln (format "nth, n * p, result: ~a ~a ~a ~a" idx n p result))
             (+ result (* n p))))
         0 (range lno slen)))

(define (pharse nthloop sigs)
  ;(displayln (format "Signals: ~a" sigs))
  (map (lambda (lno)
         (let ([pat (gen-pattern (add1 lno))])
           ;(displayln (format "Pattern: ~a" pat))
           (set! plen (length pat))
           (modulo (abs (sump sigs pat lno)) 10)))
       (range 0 (length sigs))))

(define (fft sigs n)
  (foldl pharse sigs (range 0 n)))

(define (fft-format-8 flist)
  (list->string
   (map (lambda (n)
          (first (string->list (number->string n))))
        flist)))

;(for-each (λ (n) (displayln (gen-pattern n))) (range 1 8))

(fft (parse-signal line) 4)
(fft-format-8 (fft (parse-signal "80871224585914546619083218645595") 100))
(fft-format-8 (fft (parse-signal (first (port->lines (open-input-file "inputs/d16.txt")))) 100))

