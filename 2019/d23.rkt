#lang racket

(require "utils.rkt")
(require "intcode.rkt")
(require "algo-graph.rkt")

(define node-num 50)

(define input (first (input-load-lines 23)))

; Create computers
;  One Intcode for each computer, since we don't
; support multi task in Intcode.
(define nodes
  (map (λ (addr)
         (define ic (new Intcode%))
         (send ic set-pause-on-output #t)
         (send ic set-debug #f)
         (send ic load-code input)
         (send ic set-input addr)
         ic)
       (range 0 node-num)))

; Input all packages for the node
(define (node-load-input addr ic)
  (let ([inputs (path-filter (λ (p) (= addr (first p))))])
    (cond
      [(= 0 (length inputs))
       (send ic set-input -1)
       #f]
      [else
       (when (< 1 (length inputs))
         (ddisplayln (format "Input multi packages: ~a" inputs)))
       ; Package must be processed in FIFO order
       (for ([input inputs])
         (send ic set-input (second input))
         (send ic set-input (third input))
         (path-remove input))
       #t])))

(define nat-package-found #f)
(define nat-addr 255)
(define nat-package #f)
(define nat-sent-pkgs '())  ; Save y value of sent nat packages
(define nat-2nd-y-found #f)

; Receive all packages sent out from the node
(define (node-receive ic)
  (let loop ()
    (let ([dst-addr (send ic get-output)]
          [x ((λ () (send ic run) (send ic get-output)))]
          [y ((λ () (send ic run) (send ic get-output)))])
      (cond
        ; NAT package
        [(= dst-addr nat-addr)
         (set! nat-package (list 0 x y))
         (when (not nat-package-found)
           (displayln (format "First NAT package: ~a" nat-package))
           (set! nat-package-found #t))]
        ; Common package
        [else
         (path-append (list dst-addr x y))
         (ddisplayln (format "New package: ~a" (path-peak)))]))
    ; Try to receive more packages
    (send ic run)
    (when (send ic is-pause?)
      (ddisplayln "Receive multi packages")
      (loop))))

(define (find-package break-cond)
  (let loop ()
    (define all-node-idle #t)

    ; Iterate run for each computer
    (for-each
     (λ (ic addr)
       (send ic run)
       (cond
         [(send ic is-iowait?)
          (when (node-load-input addr ic)
            (set! all-node-idle #f))]
         [(send ic is-pause?)
          (node-receive ic)]))
     nodes (range 0 node-num))

    ; Check if network is idle
    (when (and all-node-idle nat-package)
      ; Prepare activite package for next loop
      (path-push nat-package)

      ; check 2nd y
      (let ([y (third nat-package)])
        (when (member y nat-sent-pkgs)
          (displayln (format "Found 2nd y: ~a" y))
          (set! nat-2nd-y-found #t))
        (set! nat-sent-pkgs (append nat-sent-pkgs (list y)))))

    (when (not (break-cond)) (loop))))

; Part 1: 255 57557 27182
(find-package (λ () (and nat-package-found)))

; Part 2: 19285
(find-package (λ () (and nat-2nd-y-found)))
