#lang racket

(require "utils.rkt")
(require "intcode.rkt")
(require "algo-graph.rkt")

(define node-num 50)

(define input (first (input-load-lines 23)))

(define nodes
  (map (λ (addr)
         (define ic (new Intcode%))
         (send ic set-pause-on-output #t)
         (send ic set-debug #f)
         (send ic load-code input)
         (send ic set-input addr)
         ic)
       (range 0 node-num)))

(define (node-load-input addr ic)
  (let ([inputs (path-filter (λ (p) (= addr (first p))))])
    (cond
      [(= 0 (length inputs))
       (send ic set-input -1)]
      [else
       (let ([input (first inputs)])
         (send ic set-input (second input))
         (send ic set-input (third input))
         (path-remove input))])
    ))

(define addr-255-found #f)

(define (node-receive ic)
  (let ([dst-addr (send ic get-output)]
        [x ((λ () (send ic run) (send ic get-output)))]
        [y ((λ () (send ic run) (send ic get-output)))])
    (when (= dst-addr 255)
      (displayln (format "~a ~a ~a" dst-addr x y))
      (set! addr-255-found #t))
    (path-push (list dst-addr x y))
    (ddisplayln (format "New package: ~a" (path-peak)))))

(define (find-package-to-255)
  (let loop ()
    (for-each
     (λ (addr)
       (let ([ic (list-ref nodes addr)])
         (send ic run)
         (cond
           [(send ic is-iowait?)
            (node-load-input addr ic)]
           [(send ic is-pause?)
            (node-receive ic)]
           )))
     (range 0 node-num))
    (when (not addr-255-found) (loop))))

; Part 1: 255 57557 27182
(find-package-to-255)
