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

; Input all package for the node
(define (node-load-input addr ic)
  (let ([inputs (path-filter (λ (p) (= addr (first p))))])
    (cond
      [(= 0 (length inputs))
       (send ic set-input -1)
       #f]
      [else
       (when (< 1 (length inputs))
         (ddisplayln (format "Input multi packages: ~a" inputs)))
       ; WARN: The paths is in FILO (reverse order)
       (for ([input (reverse inputs)])
         (send ic set-input (second input))
         (send ic set-input (third input))
         (path-remove input))
       #t])))

(define nat-package-found #f)

(define nat-addr 255)
(define nat-package #f)
(define nat-sent-pkgs '())
(define nat-2nd-y-found #f)

; Receive all packages sent out from the node
(define (node-receive ic)
  (let loop ()
    (let ([dst-addr (send ic get-output)]
          [x ((λ () (send ic run) (send ic get-output)))]
          [y ((λ () (send ic run) (send ic get-output)))])
      (cond
        [(= dst-addr nat-addr)
         (set! nat-package (list 0 x y))
         (when (not nat-package-found)
           (displayln (format "First NAT package: ~a" nat-package))
           (set! nat-package-found #t))]
        [else
         (path-push (list dst-addr x y))
         (ddisplayln (format "New package: ~a" (path-peak)))]))
    ; Try to run again
    (send ic run)
    (when (send ic is-pause?)
      (ddisplayln "Receive multi packages")
      (loop))))

(define (find-package break-cond)
  (let loop ()
    (define all-node-idle #t)

    (for-each
     (λ (addr)
       (let ([ic (list-ref nodes addr)])
         (send ic run)
         (cond
           [(send ic is-iowait?)
            (when (node-load-input addr ic)
              (set! all-node-idle #f))]
           [(send ic is-pause?)
            (node-receive ic)])))
     (range 0 node-num))

    ; Check if network is idle
    (when (and all-node-idle nat-package)
      (let ([x (second nat-package)]
            [y (third nat-package)])
        ; Prepare activite package for next loop
        (path-push nat-package)
        ; check 2nd y
        (when (member y nat-sent-pkgs)
          (displayln (format "Found 2nd y: ~a" y))
          (set! nat-2nd-y-found #t))
        (set! nat-sent-pkgs (append nat-sent-pkgs (list y)))))

    (when (not (break-cond)) (loop))))

; Part 1: 255 57557 27182
(find-package (λ () (and nat-package-found)))

; Part 2: 19285
(find-package (λ () (and nat-2nd-y-found)))
