#lang racket

(require "utils.rkt")

(define input (input-load-lines 22))

(define card-num 10)
(define cards #f)

(define (cut n)
  (ddisplayln (format "Cut ~a" n))
  (let ([clist (vector->list cards)]
        [len (if (positive? n) n (+ card-num n))])
    (set! cards (list->vector (append (drop clist len) (take clist len))))))

(define (new-stack)
  (ddisplayln "New stack")
  (set! cards (list->vector (reverse (vector->list cards)))))

(define (incr n)
  (ddisplayln (format "Incr ~a" n))
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

;(test "22-1")
;(test "22-2")
;(test "22-3")
;(test "22-4")

; Find out position of card 2019
(define (printh nth)
  (for-each
   (λ (idx)
     (when (= nth (vector-ref cards idx))
       (displayln (format "~a card at: ~a" nth idx))))
   (range 0 card-num)))

; Part 1
(set! card-num 10007)
;(test "22")
;(printh 2019)

(define (shuff-tech->ops name)
  (map (λ (line)
         (let ([strs (string-split line " ")])
           ;(displayln strs)
           (cond
             [(string=? "cut" (first strs))
              (let ([n (string->number (second strs))]) 
                (list 0 (if (positive? n) n (+ card-num n))))]
             [(string=? "into" (second strs))
              (list 1 0)]
             [(string=? "with" (second strs))
              (list 2 (string->number (list-ref strs 3)))])))
       (input-load-lines name)))

(define (shuff-cut n val)
  (if (>= n val)
      (- n val)
      (+ n (- card-num val))))

(define card-num-half (truncate (/ card-num 2)))

(define (shuff-new-stack n val)
  (let ([off (- n card-num-half)])
    (- n off off)))

(define (shuff-increment n val)
  (modulo (* n val) card-num))

(define (part1-v2)
  (define pos-2019 2019)

  (for-each (λ (shuff)
              (let ([op (first shuff)]
                    [val (second shuff)])
                (set! pos-2019 
                      (cond
                        [(= op 0) (shuff-cut pos-2019 val)]
                        [(= op 1) (shuff-new-stack pos-2019 val)]
                        [(= op 2) (shuff-increment pos-2019 val)]))
                (ddisplayln (format "Shuff ~a ~a" shuff pos-2019))
                ))
            (shuff-tech->ops 22))

  (displayln (format "2019 at: ~a" pos-2019)))

; Part1 v2
(part1-v2)

(define n2 119315717514047)
(define nl 101741582076661)

