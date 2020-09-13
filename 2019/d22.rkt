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

(define (shuff-cut-r n val)
  (modulo (+ n val) card-num))

(define card-num-half (truncate (/ card-num 2)))

(define (set-card-num! n)
  (set! card-num n)
  (set! card-num-half (truncate (/ card-num 2))))

(define (shuff-new-stack n val)
  (let ([off (- n card-num-half)])
    (- n off off)))

(define (shuff-increment n val)
  (modulo (* n val) card-num))

; TODO: Optimize out of div/mul
(define (shuff-increment-r n val)
  (let loop ([x 1])
    ;(displayln (format "Loop ~a" x))
    (let ([y (+ n (* card-num x))])
      (if (= 0 (modulo y val))
          (/ y val)
          (loop (add1 x))))))

(define shuffs (shuff-tech->ops 22))

(define pos-orig 2019)
(define pos-new 2019)
(define (set-pos! n)
  (set! pos-orig n)
  (set! pos-new n))

(define (part1-v2 n)
  (set-pos! n)
  (for-each (λ (shuff)
              (let ([op (first shuff)]
                    [val (second shuff)])
                (set! pos-new 
                      (cond
                        [(= op 0) (shuff-cut pos-new val)]
                        [(= op 1) (shuff-new-stack pos-new val)]
                        [(= op 2) (shuff-increment pos-new val)]))
                ;(ddisplayln (format "Shuff ~a ~a" shuff pos-new))
                ))
            shuffs)
  ;(displayln (format "~a at: ~a" pos-orig pos-new))
  )

; Part1 v2
(part1-v2 2019)
(displayln (format "~a at: ~a" pos-orig pos-new))

; Find forward roll back count
(define (find-fwd-roll-back shuff n)
  (set-pos! n)
  (let loop ([idx 1])
    ;(when (< idx 10) (displayln (format "~a ~a" pos-orig pos-new)))
    (when (= 0 (modulo idx 10000000)) (display "m") (flush-output))
    (shuff pos-new)
    (if (= n pos-new)
        (displayln (format "Looped after ~a round" idx))
        (loop (add1 idx)))))
; Result: each pos will return to orignal after card-num of shuffling
(find-fwd-roll-back part1-v2 2019)

(define (part1-v2-reverse n)
  (set-pos! n)
  (for-each (λ (shuff)
              (let ([op (first shuff)]
                    [val (second shuff)])
                (set! pos-new 
                      (cond
                        [(= op 0) (shuff-cut-r pos-new val)]
                        [(= op 1) (shuff-new-stack pos-new val)]
                        [(= op 2) (shuff-increment-r pos-new val)]))
                ;(ddisplayln (format "Shuff ~a ~a" shuff pos-new))
                ))
            (reverse shuffs))
  ;(displayln (format "~a from: ~a" pos-orig pos-new))
  )

;(displayln "Reverse shuff")
(part1-v2-reverse 2020)
(displayln (format "~a from: ~a" pos-orig pos-new))
(find-fwd-roll-back part1-v2-reverse 2020)

(define card-num-giant 119315717514047)
(define shufflee-giant 101741582076661)

(define rollback-steps (- card-num-giant shufflee-giant))

;(set-card-num! card-num-giant)
;(part1-v2-reverse 2020)

