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
(define (set-steps! s) (set! steps s))

(define A '())
(define B '())
(define C '())
(define MAIN '())

(define clist '())
; The max step clip
(define clip-max 10)
(define clip-min 2)

(define (clips-reset)
  (set! A '())
  (set! B '())
  (set! C '())
  (set! clist '()))

; List is equal when
;  1. Each item at same index is same and
;  2. The list length is equal.
(define (list-eq? a b)
  (and (= (length a) (length b))
       (andmap (λ (v1 v2) (eq? v1 v2)) a b)))

; List A/B has B/A
;  1. Lenght of A/B is different
;  2. A is a sub set of B at position 0 or vis versa
(define (list-has? a b)
  (let* ([la (length a)]
         [lb (length b)]
         [len (min la lb)])
    (and (not (= la lb))
         (list-eq? (take a len) (take b len)))))

; Every sub path in the clist is equal?
(define (clist-is-equal?)
  (andmap
   (λ (idx)
     (list-eq? (list-ref clist idx)
               (list-ref clist (add1 idx))))
   (range 0 (sub1 (length clist)))))

; Find out the max sub clip for C
(define (clist-split-check cc)
  ; TODO: do we need to check the length of cc first?
  (ormap (λ (n)
           (cond
             [(> (modulo (length cc) n) 0) #f]
             [else
              (set! clist '())
              ;(displayln (format "Clip ~a: ~a" n cl))
              (for ([idx (range 0 (/ (length cc) n))])
                (set! clist (append clist (list (take (drop cc (* idx n)) n)))))
              (set! C (first clist))
              (clist-is-equal?)]))
         (range clip-max 0 -2)))

; Check if we found a valid C, or need to split into small one
(define (recheck-clist)
  (cond
    [(= 0 (length clist)) #t]
    [(or (= 1 (length clist))
         (clist-is-equal?))
     (if (> (length C) clip-max)
         (clist-split-check C)
         #t)]
    [else #f]))

; Return the pos of the sub path, or #f if not found
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
         (loop (+ sstart clip-min))]))))

; Remove sub path from steps recursively
; If save-c is #t, save remaind paths into clist
; return remained steps in s
(define (drop-steps s a save-c)
  (let loop ([steps-new s])
    (let ([ret (find-steps steps-new a)])
      (cond
        [(and ret)
         (cond
           [(and save-c (> ret 0))
            ; Save sub path remained
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
; save reaminded sub paths into clist,
; check if the clist is valid sub path (len < 10)
; or repeat of valid sub path.
;
(define (find-great-clips)
  (let loop ([la clip-min]
             [lb clip-min]
             [s steps])
    (set! clist '())
    (ddisplayln (format "Find clip: ~a ~a" la lb))

    (let ([l (length s)])
      (set! A (take s la))
      (set! B (drop s (- l lb)))
      (ddisplayln (format "Try A ~a, B ~a" A B))
      (cond
        [(list-eq? A B)
         (displayln (format "Remove tail B: ~a" B))
         (loop la clip-min (take s (- l lb)))]
        ; Continue search even if A/B is similar
        ;[(or (list-has? A B)
        ;     (list-has? B A))
        ; (displayln (format "Intersect of A/B: ~a, ~a" A B))
        ; #f]
        ;[(or (find-steps A B)
        ;     (find-steps B A))
        ; (displayln (format "Included of A/B: ~a, ~a" A B))
        ; #f]
        [else
         (let ([sremaind (drop-steps (drop-steps steps A #f) B #t)])
           (if (and (= (length sremaind) 0)
                    (recheck-clist))
               (displayln (format "Found clip: A: ~a, B: ~a, C: ~a" A B C))
               (cond
                 [(and (= lb clip-max) (= la clip-max)) #f]
                 [(and (= lb clip-max) (< la clip-max))
                  (loop (+ la clip-min) clip-min s)]
                 [else
                  (loop la (+ lb clip-min) s)])))])
      )))

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
        [else
         (displayln (format "Failed to match ~a with A/B/C" s))
         (displayln (format "A: ~a, B: ~a, C: ~a" A B C))
         prog]))))

(define (clips-find)
  (find-great-clips)
  (set! MAIN (convert-steps-to-prog))
  (ddisplayln (format "Main in module: ~a" MAIN)))

(define (test-steps s)
  (set-steps! s)
  (clips-reset)
  (clips-find)
  (displayln (format "Prog: ~a" (convert-steps-to-prog))))

(module+ main #f
  (test-steps '(R 12 L 8 L 4))
  (test-steps '(R 12 L 8 L 4 L 4))
  (test-steps '(R 12 L 8 L 4 L 4 L 8 R 6 L 6 R 12 L 8
                  L 4 L 4 L 8 R 6 L 6 L 8 L 4 R 12
                  L 6 L 4 R 12 L 8 L 4 L 4 L 8 L 4
                  R 12 L 6 L 4 R 12 L 8 L 4 L 4 L 8
                  L 4 R 12 L 6 L 4 L 8 R 6 L 6))

  (test-steps '(R 12 L 8 L 4 L 4 L 8 R 6 L 6 R 12 L 8
                  L 4 L 4 L 8 R 6 L 6 L 8 L 4 R 12
                  L 6 L 4 R 12 L 8 L 4 L 4 L 8 L 4
                  R 12 L 6 L 4 R 12 L 8 L 4 L 4 L 8
                  L 4 R 12 L 6 L 4 L 8 R 6 L 6
                  R 12 L 8 L 4 L 4))
  )