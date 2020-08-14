#lang racket

(require "intcode.rkt")
(require "input.rkt")

(define input (first (input-load-lines 15)))

(define dbg #f)

(define (ddisplayln msg)
  (when dbg
    (displayln msg)))

; Direction
(define mn 1) ; North
(define ms 2) ; South
(define mw 3) ; West
(define me 4) ; East

; Stage blocks index
(define WALL 0)
(define ROAD 1)
(define OXYG 2)
(define DROD 3)
(define EMPT 4)
(define stage-strs (list  "#" "." "@" "D" " "))

; Clock wise
(define move-delta1
  ; Move  x,y,direction,index
  (list '(0 -1 1 0) ; North
        '(1  0 4 1) ; East
        '(0  1 2 2) ; South
        '(-1 0 3 3) ; West
        ))

; Counter lock wise
(define move-delta2
  ; Move  x,y,direction,index
  (list '(0 -1 1 0) ; North
        '(-1 0 3 1) ; West
        '(0  1 2 2) ; South
        '(1  0 4 3) ; East
        ))

(define move-delta move-delta2)

; Choose move back direction
(define (get-move-back-dir dir)
  (let ([idx (list-ref dir 3)])
    (list-ref move-delta (modulo (+ idx 2) 4))))

(define ic (new Intcode%))
(send ic set-pause-on-output #t)
;(send ic set-debug #t)

(define h 42)
(define v h)

; Init position
(define x (/ h 2))
(define y (/ v 2))

(define (reset-xy)
  (set! x (/ h 2))
  (set! y (/ v 2)))

; Oxygen System position
(define ox 0)
(define oy 0)

; Stage
(define stage (make-vector (* h v) EMPT))

; How many times we stepped on this point
(define stage-weight2 (make-vector (* h v) 0))
(define stage-weight2-strs (list  "." "A" "B" "C" "D" "E" "F" "G" "H"))

; Init path list
; paths item (list x y))
(define paths '())
(define (path-reset) (set! paths '()))
(define (path-empty) (= 0 (length paths)))
(define (path-push a) (set! paths (append (list a) paths)))
(define (path-peak) (car paths))
(define (path-peak2) (car (cdr paths)))
(define (path-len) (length paths))
(define (path-dump-len) (displayln (format "Path len: ~a" (path-len))))

(define (path-pop)
  (let ([a (car paths)])
    (set! paths (cdr paths))
    a))

(define (path-equal a b)
  (and (= (list-ref a 0) (list-ref b 0))
       (= (list-ref a 1) (list-ref b 1))))

(define (path-add x y)
  (cond
    [(path-empty) (path-push (list x y))]
    [(and (> (path-len) 1)
          (path-equal (path-peak2) (list x y)))
     ;(displayln (format "Path fallback: ~a ~a" x y))
     (path-pop)]
    [else (path-push (list x y))]))
     
; Update current position
; and reduce the weight of the passed position for DFS?/BFS?
(define (update-pos dir ret update-axis)
  (let* ([axis-off dir]
         [nx (+ x (first axis-off))]
         [ny (+ y (second axis-off))]
         [idx (+ nx (* ny h))])
    (vector-set! stage idx ret)
    (when update-axis
      (when (not (= ret WALL))
        (set! x nx)
        (set! y ny)
        (path-add x y))
      (vector-set! stage-weight2 idx
                   (sub1 (vector-ref stage-weight2 idx))))))

; Intcode move
(define (intcode-move dir)
  (send ic set-input (third dir))
  (send ic run)
  (send ic get-output))

; Move
(define (move dirs)
  (let ([dir (find-weightest-dir dirs)])
    (let ([ret (intcode-move dir)])
      (ddisplayln (format "Move ~a, ret ~a" dir ret))
      (cond
        [(= ret WALL)
         ;(displayln "It's wall")
         (update-pos dir ret #f)]
        [(= ret ROAD)
         (retrofit dir dirs)
         (update-pos dir ret #t)
         (ddisplayln (format "Moved ~a ~a" x y))]
        [(= ret OXYG)
         (update-pos dir ret #t)
         ;(dump-stage)
         (ddisplayln (format "Moved ~a ~a" x y))
         (displayln (format "Found Oxygen: ~a ~a" x y))
         (set! ox x)
         (set! oy y)])
      ret)))

; Find all open dirs in types in a list of
;  '(weight dir)
(define (find-dir-open-at x y dir-types)
  (foldl
   (lambda (dir result)
     (append result
             (let* ([nx (+ x (first dir))]
                    [ny (+ y (second dir))]
                    [idx (+ nx (* ny h))]
                    [c (vector-ref stage idx)]
                    [w2 (vector-ref stage-weight2 idx)])
               ;(ddisplayln (format "Find ~a ~a ~a ~a" nx ny c w2))
               (cond
                 [(member c dir-types) (list (list (+ c w2) dir))]
                 [else '()]))))
   '() move-delta))

(define (dump-stage)
  ; Print cabinet
  (for ([i (range 0 h)]) (display "-"))
  (displayln "")

  (let ([didx (+ x (* y h))])
    (for ([i (range 0 (* h v))])
      (let ([c (vector-ref stage i)])
        (if (= i didx)
            (display "D")
            (display (list-ref stage-strs c))))
      (when (= (modulo i h) (sub1 h)) (displayln ""))))
  (displayln ""))

(define (dump-stage-weight2)
  (displayln "Dump stage weight")
  ; Print cabinet
  (for ([i (range 0 h)]) (display "-"))
  (displayln "")

  (for ([i (range 0 (* h v))])
    (let ([c (vector-ref stage-weight2 i)])
      (display (list-ref stage-weight2-strs (abs c))))
    (when (= (modulo i h) (sub1 h)) (displayln "")))
  (displayln ""))


; Select way by weight
(define (find-weightest-dir dirs)
  (define dir (first dirs))
  (for-each
   (lambda (d)
     (when (> (first d) (first dir))
       (set! dir d)))
   (cdr dirs))
  (second dir))

; Explore more blocks before move forward
(define (retrofit dir dirs)
  ; Move back
  (intcode-move (get-move-back-dir dir))

  ; Try each dirs
  (for-each
   (lambda (d)
     (when (not (= (list-ref dir 3)
                   (list-ref d 3)))
       (let ([ret (intcode-move d)])
         (ddisplayln (format "Retrofit Move ~a, ret ~a" d ret))
         (cond
           [(= ret WALL)
            ;(displayln "It's wall")
            (update-pos d ret #f)]
           [(= ret ROAD)
            (update-pos d ret #f)
            ; Move back
            (intcode-move (get-move-back-dir d))]
           [(= ret OXYG)
            (update-pos d ret #f)
            ; Move back
            (intcode-move (get-move-back-dir d))
            ;TODO Do some thing
            (displayln (format "Found Oxygen - Retrofit: ~a ~a" x y))
            (set! ox x)
            (set! oy y)])
         ;(dump-stage)
         )))
   (map second dirs))

  ; Move forward
  (intcode-move dir))

(define (go)
  (send ic load-code input)
  (let loop ()
    ;(displayln "Loop")
    (let ([dirs (find-dir-open-at x y (list EMPT ROAD OXYG))])
      (cond
        [(= 0 (length dirs)) (displayln "Dead end")]
        [else
         (ddisplayln (format "Dirs x ~a y ~a, ~a" x y dirs))
         ;(when (= 1 (length dirs)) (displayln "Dead end found, fall back"))
         (let ([ret (move dirs)])
           (when (not (= OXYG ret))
             ;(dump-stage)
             ;(flush-output)
             ;(sleep 0.04)
             (loop)))]))))

; Part 1
(define (part1)
  (set! move-delta move-delta1)
  (go)
  (reset-xy) ; Move D to original position
  (dump-stage)
  ;(dump-stage-weight2)
  (path-dump-len))

(part1)

; Part 2
; Fill all the area with oxygen gradually
(define open-dirs (list (list ox oy)))

(define (pos-move-to x y dir)
  (list (+ x (first dir)) (+ y (second dir))))

(define (fill-oxygen)
  (define new-dirs '())
  (for-each (λ (o)
              (let* ([x (first o)]
                     [y (second o)]
                     [idx (+ x (* y h))]
                     [dirs (find-dir-open-at x y (list ROAD))])
                ;(displayln (format "Open dirs at ~a ~a: ~a" x y dirs))
                (vector-set! stage idx OXYG)
                (for-each (λ (d)
                            (set! new-dirs (append new-dirs (list (pos-move-to x y (second d))))))
                          dirs)))
            open-dirs)
  (set! open-dirs new-dirs))

(define (go-fill)
  (let loop ()
    (cond
      [(empty? open-dirs)
       (displayln (format "Full filled in ~a steps" (path-len)))]
      [else
       (fill-oxygen)
       (when (not (empty? open-dirs))
         (path-push (list (length open-dirs) open-dirs)))
       (when (= 0 (modulo (path-len) 1))
         (dump-stage)
         (flush-output)
         (sleep 0.2))
       (loop)])))

;Try to find all road & wall
; Round 2 to explore full map
; WARN: Assume the max branch number is 2.
;(set! move-delta move-delta2) ; another option to explore full map
(define (part2)
  ; Depends on part 1
  (path-reset)
  (go)
  (reset-xy)
  (dump-stage)
  (path-dump-len)

  (path-reset)
  (go-fill)
  (dump-stage))

(part2)
