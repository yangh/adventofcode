#lang racket

(require "intcode.rkt")
(require "input.rkt")

(define input (first (input-load-lines 7)))

(define (build-amp input)
  (define ic (new Intcode%))
  (send ic load-code input)
  ;(send ic set-pause-on-output #t)
  ;(send ic set-debug #t)
  ic)

(define (is-all-diff? a b c d e)
  (define v (make-hash))
  (hash-set! v a a)
  (hash-set! v b b)
  (hash-set! v c c)
  (hash-set! v d d)
  (hash-set! v e e)
  (= (hash-count v) 5))

(define (test input phases)
  (define amps
    (list
     (build-amp input)
     (build-amp input)
     (build-amp input)
     (build-amp input)
     (build-amp input)))
  (define phs (map string->number (string-split phases ",")))
  (define o 0)

  (for ([i (range 0 5)])
    (let ([ic (list-ref amps i)]
          [ph (list-ref phs i)])
      (send ic set-input ph)
      (send ic run)
      ))

  (let loop ()
    (for ([i (range 0 5)])
      (let ([ic (list-ref amps i)])
        (when (not (send ic is-halt?))
          (send ic set-input o)
          (send ic run)
          (set! o (send ic get-output))
          ;(displayln o)
          )))
    (when (not (send (list-ref amps 4) is-halt?))
      (loop)))
  o)

;(test "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" "9,8,7,6,5")

(define (part2 input)
  (define m 0)
  (define amps "")
  (for* ([a (range 5 10)]
         [b (range 5 10)]
         [c (range 5 10)]
         [d (range 5 10)]
         [e (range 5 10)])
    ;Each pharse used exactly once
    (when (is-all-diff? a b c d e)
      (let* ([as (format "~a,~a,~a,~a,~a" a b c d e)]
             [n (test input as)])
        (when (> n m)
          (set! m n)
          (set! amps as)
          (displayln m)
          ))))
  (displayln (format "Max thruster: ~a from ~a" m amps))
  m)

;(part2 "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
(part2 input)