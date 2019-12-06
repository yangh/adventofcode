#lang racket

(require "input.rkt")

(define input (input-load-lines 'd6))

(struct planet (name orbith count) #:mutable)

(define planets-hash (make-hash))

(define (orbit-add name)
  (when (not (hash-has-key? planets-hash name))
    (hash-set! planets-hash name (planet name (make-hash) 0)))
  (hash-ref planets-hash name))

; Load planets
(for-each
 (lambda (line)
   (define orb (string-split line ")"))
   (let ([orb1 (orbit-add (first orb))]
         [orb2 (orbit-add (second orb))])
     (hash-set! (planet-orbith orb1)
                (planet-name orb2) orb2)
     ;(set-orbit-count! orb2 (add1 (orbit-count orb1)))
     ))
 input)

(define (dump-orbit)
  (hash-for-each
   planets-hash
   (lambda (k o)
     (displayln (format "~a, ~a" (planet-name o) (planet-count o))))))

; count orbit
(let loop ([o (hash-ref planets-hash "COM")])
  (hash-for-each (planet-orbith o)
                 (lambda (k so)
                   (set-planet-count! so (add1 (planet-count o)))
                   (loop so))))

(dump-orbit)

; count total
(foldl (lambda (o r)
         (+ r (planet-count o)))
       0 (hash-values planets-hash))

