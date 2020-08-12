#lang racket

(define line "12345678")

(define (parse-signal line)
  (map (lambda (c) (string->number(string c))) (string->list line)))

(define signals (parse-signal line))

(define pattern (list 0 1 0 -1))

(define (gen-pattern n)
  (foldl (lambda ( v result)
           (append result (make-list n v)))
         '() pattern))

;(gen-pattern 1)
;(gen-pattern 2)
;(gen-pattern 3)

(define (sump sig pat)
  (let* ([slen (length sig)]
         [plen (length pat)]
         [out (make-list slen 0)])
    (foldl (lambda (n idx result)
             (let ([p (list-ref pat (modulo idx plen))])
               ;(displayln (format "n * p, result: ~a ~a ~a" n p result))
               (+ result (* n p))))
           0 sig (range 1 (add1 slen)))))

(define (pharse idx sigs)
  (map (lambda (idx)
         (modulo (abs (sump sigs (gen-pattern idx))) 10))
       (range 1 (add1 (length sigs)))))

(define (fft sigs n)
  (foldl pharse sigs (range 1 (add1 n))))

;(fft (parse-signal line) 4)

(define (fft-format-8 flist)
  (list->string
   (map (lambda (n)
          (first (string->list (number->string n))))
        flist)))

(fft-format-8 (fft (parse-signal "80871224585914546619083218645595") 100))

(fft-format-8 (fft (parse-signal (first (port->lines (open-input-file "inputs/d16.txt")))) 100))

