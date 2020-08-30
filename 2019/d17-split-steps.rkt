#lang racket

(require "utils.rkt")

(provide (all-defined-out))

;;;
; Find out 3 paths to cover all the steps by algorithm
; 
;  1. Each sub path length < 10
;
; Input steps:
;
;(R 12 L 8 L 4 L 4      < A
;   L 8 R 6 L 6         < B
; R 12 L 8 L 4 L 4
;   L 8 R 6 L 6
; L 8 L 4 R 12 L 6 L 4  < C
; R 12 L 8 L 4 L 4
; L 8 L 4 R 12 L 6 L 4
; R 12 L 8 L 4 L 4
; L 8 L 4 R 12 L 6 L 4
;   L 8 R 6 L 6)
;
; Main (converted steps):
;   A B A B C A C A C B
;;;

(define steps '())
(define (set-steps! s)
  (set! steps s))

(define A '())
(define B '())
(define C '())

(define clist '())
; The max step clip
(define clip-max 10)

(define (list-eq? a b)
  (let ([la (length a)]
        [lb (length b)])
    (and (= la lb)
         (andmap
          (λ (v1 v2) (eq? v1 v2)) a b))))

(define (clist-is-equal?)
  (andmap
   (λ (idx)
     (list-eq? (list-ref clist idx)
               (list-ref clist (add1 idx))))
   (range 0 (- (length clist) 1))))

; Find out the max clip for C
(define (clist-split-check lc)
  (ormap (λ (n)
           (cond
             [(> (modulo (length lc) n) 0) #f]
             [else
              (set! clist '())
              ;(displayln (format "Clip ~a: ~a" n lc))
              (for ([idx (range 0 (/ (length lc) n))])
                (set! clist (append clist (list (take (drop lc (* idx n)) n)))))
              (set! C (first clist))
              (clist-is-equal?)]))
         (range clip-max 0 -2)))

(define (recheck-clist)
  (cond
    [(= 0 (length clist)) #t]
    [(= 1 (length clist))
     (set! C (first clist))
     (if (> (length C) clip-max)
         (clist-split-check C)
         #t)]
    [(clist-is-equal?) (clist-split-check C)]
    [else #f]))

(define (find-steps s a)
  (let ([send (- (length s) (length a))]
        [la (length a)])
    (let loop ([sstart 0])
      (cond
        [(> sstart send)
         ;(displayln "Find reach end")
         #f]
        [(list-eq? a (drop (take s (+ sstart la)) sstart))
         sstart]
        [else
         (loop (+ sstart 2))]))))

(define (drop-steps s a save-c)
  (let loop ([steps-new s])
    (let ([ret (find-steps steps-new a)])
      (cond
        [(and ret)
         (cond
           [(and save-c (> ret 0))
            ; Save ramind clips
            (set! clist (append clist (list (take steps-new ret))))
            (set! C (first clist))
            (loop (drop steps-new (+ ret (length a))))]
           [else
            (let ([steps-x (append (take steps-new ret)
                                   (drop steps-new (+ ret (length a))))])
              (loop steps-x))])]
        [else steps-new]))))

;
; Define A/B from the head/tail of the steps,
; then remove A/B sub paths from the steps,
; save reaminded usb paths into clist,
; check if the clist is valid sub path (len < 10)
; or repeated valid sub path.
;
(define (good-path a b)
  (set! clist '())
  (let loop ([la a]
             [lb b]
             [s steps])
    (let ([l (length s)])
      (set! A (take s la))
      (set! B (drop s (- l lb)))
      ;(displayln (format "Try A ~a, B ~a" A B))
      (cond
        [(eq? A B)
         (set! B (drop s (- l lb lb)))
         (loop la lb (take s (- l lb)))]
        [else
         (drop-steps (drop-steps steps A #f) B #t)
         (recheck-clist)])
      )))

(define (find-good-clips)
  (define found #f)
  (for* ([la (range 2 12 2)]
         [lb (range 2 12 2)]
         #:break (and found))
    (set! clist '())
    (when (and (good-path la lb))
      (set! found #t)
      ;(displayln (format "Clip: ~a ~a" la lb))
      (displayln (format "Found clip: A: ~a, B: ~a, C: ~a" A B C)))))

;(good-path 8 6)
;(find-good-clips)

(define (convert-steps-to-prog)
  (let ([la (length A)]
        [lb (length B)]
        [lc (length C)])
    (let loop ([s steps]
               [prog '()])
      ;(displayln (format "Prog: ~a" prog))
      (cond
        [(= 0 (length s)) prog]
        [(and (>= (length s) la) (list-eq? A (take s la)))
         (loop (drop s la) (append prog (list #\A)))]
        [(and (>= (length s) lb) (list-eq? B (take s lb)))
         (loop (drop s lb) (append prog (list #\B)))]
        [(and (>= (length s) lc) (list-eq? C (take s lc)))
         (loop (drop s lc) (append prog (list #\C)))]
        [else prog]))))

(define MAIN '())

(define (clips-find)
  (find-good-clips)
  (set! MAIN (convert-steps-to-prog))
  (ddisplayln (format "Main in module: ~a" MAIN)))

(module+ main #f
  (set-steps! '(R 12 L 8 L 4 L 4 L 8 R 6 L 6 R 12 L 8
                  L 4 L 4 L 8 R 6 L 6 L 8 L 4 R 12
                  L 6 L 4 R 12 L 8 L 4 L 4 L 8 L 4
                  R 12 L 6 L 4 R 12 L 8 L 4 L 4 L 8
                  L 4 R 12 L 6 L 4 L 8 R 6 L 6))
  (clips-find)
  (displayln (format "Prog: ~a" (convert-steps-to-prog))))