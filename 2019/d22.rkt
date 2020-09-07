#lang racket

(require "utils.rkt")

(define input (input-load-lines 22))

(define card-num 10)
(define cards #f)

(define (cut n)
  (displayln (format "Cut ~a" n))
  (let ([clist (vector->list cards)]
        [len (if (positive? n) n (+ card-num n))])
    (set! cards (list->vector (append (drop clist len) (take clist len))))))

(define (new-stack)
  (displayln "New stack")
  (set! cards (list->vector (reverse (vector->list cards)))))

(define (incr n)
  (displayln (format "Incr ~a" n))
  ; TODO: remove cbufs, move in place

  (define idx 0)
  (for-each
   (λ (card)
     (vector-set! cards idx card)
     (set! idx (modulo (+ idx n) card-num)))
   (vector->list cards)))

(define (test name)
  (set! cards (list->vector (range 0 card-num)))

  (for-each
   (λ (str)
     (let ([strs (string-split str " ")])
       ;(displayln strs)
       (cond
         [(string=? "cut" (first strs))
          (cut (string->number (second strs)))]
         [(string=? "into" (second strs))
          (new-stack)]
         [(string=? "with" (second strs))
          (incr (string->number (list-ref strs 3)))])))
   (input-load-lines name))

  (ddisplayln cards))

(test "22-1")
(test "22-2")
(test "22-3")
(test "22-4")

(set! card-num 10007)
(test "22")

; Find out position of card 2019
(for-each
 (λ (idx)
   (when (= 2019 (vector-ref cards idx))
     (displayln (format "2019 card at: ~a" idx))))
 (range 0 card-num))