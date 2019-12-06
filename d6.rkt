#lang racket

(require "input.rkt")

(define input (input-load-lines 'd6))

(struct planet (name parent orbith count) #:mutable)

(define planets-hash (make-hash))

(define (planet-add h name)
  (when (not (hash-has-key? h name))
    (hash-set! h name (planet name #f (make-hash) 0)))
  (hash-ref h name))

; Load planets
(for-each
 (lambda (line)
   (define orb (string-split line ")"))
   (let ([orb1 (planet-add planets-hash (first orb))]
         [orb2 (planet-add planets-hash (second orb))])
     (hash-set! (planet-orbith orb1)
                (planet-name orb2) orb2)
     (set-planet-parent! orb2 orb1)
     ))
 input)

(define (dump-orbit h)
  (hash-for-each
   h
   (lambda (k o)
     (displayln (format "~a, ~a" (planet-name o) (planet-count o))))))

; count orbit
(let loop ([o (hash-ref planets-hash "COM")])
  (hash-for-each (planet-orbith o)
                 (lambda (k so)
                   (set-planet-count! so (add1 (planet-count o)))
                   (loop so))))

;(dump-orbit planets-hash)

; count total, part 1
(foldl (lambda (o r)
         (+ r (planet-count o)))
       0 (hash-values planets-hash))

; part 2
(define you (hash-ref planets-hash "YOU"))
(define san (hash-ref planets-hash "SAN"))

(define paths (make-hash))

; Find the pato to COM, build a new hash table
(define (find-path-to-com startp)
  (let loop ([pp (planet-parent startp)]
             [last-p startp])
    (let ([newp (planet-add paths (planet-name pp))])
      ; Add last-p to newps orbit
      (when last-p
        (hash-set! (planet-orbith newp)
                   (planet-name last-p) last-p))
      (when (not (string=? "COM" (planet-name pp)))
        (loop (planet-parent pp) newp)))))

(find-path-to-com you)

; count orbit in paths
(let loop ([o (hash-ref paths "COM")])
  (hash-for-each
   (planet-orbith o)
   (lambda (k so)
     (set-planet-count! so (add1 (planet-count o)))
     (displayln (format "~a, ~a" (planet-name o) (planet-count o)))
     (loop so))))

(hash-count paths)

; Find joint path
(let loop ([o (planet-parent san)]
           [n 0])
  (cond
    [(hash-has-key? paths (planet-name o))
     (displayln (format "Found joint: ~a/~a, step ~a" (planet-name o) (planet-count o)  n))
     (let ([jointp (hash-ref paths (planet-name o))])
       (+ n (- (hash-count paths) (planet-count jointp) 1)))]
    [else
     (loop (planet-parent o) (add1 n))]))

