#lang racket

(require "input.rkt")

(define input (first (input-load-lines 5)))

(define (input->vector input)
  (list->vector
   (map string-trim
        (string-split input ","))))

; Immediate mode parameter
(define (number-at ints pos)
  (string->number
   (vector-ref ints pos)))

; Position mode parameter, var
(define (value-at ints pos)
  (number-at ints (number-at ints pos)))

; Load parameters in 0/1 mode
(define (load-param ints op pos nth)
  (if (or
       ; No mode in opcode, Position mode as default
       (< (string-length op) (+ 2 nth))
       ; 0 - Possition mode
       (char=? #\0 (string-ref op (- (string-length op) nth 2))))
      (value-at ints (+ pos nth))
      ; 1 - Immediate mode
      (number-at ints (+ pos nth))))

; add/mul src1 src2 dist
(define (op12 func ints pos)
  (let* ([op (vector-ref ints pos)]
         [n1 (load-param ints op pos 1)]
         [n2 (load-param ints op pos 2)]
         [n3 (number-at ints (+ pos 3))])
    (displayln (format "OP: ~a ~a ~a ~a" op n1 n2 n3))
    (vector-set! ints n3 (format "~a" (func n1 n2))))
  ; Forward
  (intcode ints (+ pos 4)))

(define (value-set! ints pos value)
  (displayln (format "Set: ~a ~a" pos value))
  (vector-set! ints pos value))

; store dst <input>
(define (op3 ints pos value)
  (value-set! ints (number-at ints (+ pos 1)) value)
  (intcode ints (+ pos 2)))

; output src
(define (op4 ints pos)
  (let* ([op (vector-ref ints pos)]
         [n1 (load-param ints op pos 1)])
    (displayln (format "op4: ~a" n1)))
  (intcode ints (+ pos 2)))

; jump-if-ture/false
(define (jmp-if cond ints pos)
  (let* ([op (vector-ref ints pos)]
         [n1 (load-param ints op pos 1)]
         [n2 (load-param ints op pos 2)])
    (displayln (format "jump-if: ~a ~a" n1 n2))
    (if (cond n1 0) n2 0)))

(define ne? (lambda (n1 n2) (not (= n1 n2))))

; jump to n2 if n1 != 0
(define (op5 ints pos)
  (jmp-if ne? ints pos))

; jump to n2 if n1 == 0
(define (op6 ints pos)
  (jmp-if = ints pos))

(define (jmp-op ints pos op)
  (let ([ret (op ints pos)])
    (if (= ret 0)
        (intcode ints (+ pos 3))
        (intcode ints ret))))

; compare, lt, eq
(define (cmp-if cond ints pos)
  (let* ([op (vector-ref ints pos)]
         [n1 (load-param ints op pos 1)]
         [n2 (load-param ints op pos 2)])
    (displayln (format "cmp-if: ~a ~a" n1 n2))
    (cond n1 n2)))

; cmp, set n3 = (n1 < n2) ? 1 : 0
(define (op7 ints pos)
  (cmp-if < ints pos))

; cmp, set n3 = (n1 == n2) ? 1 : 0
(define (op8 ints pos)
  (cmp-if = ints pos))

(define (cmp-op ints pos op)
  (let ([n3 (number-at ints (+ pos 3))]
        [value (if (op ints pos) "1" "0")])
    (value-set! ints n3 value)
    (intcode ints (+ pos 4))))

(define system-id "1")

; check op: ABCDE
;    DE: op
;    ABC: parameter mode, 0 - position, 1 - immediate
(define (is-op? op code)
  (char=? (last (string->list op)) code))

; Intcode computer
(define (intcode ints pos)
  (when (< pos (vector-length ints))
    (let ([op (vector-ref ints pos)])
      ;(displayln (format "~a" ints))
      (displayln (format "pos: ~a" pos))
      (displayln (format "opc: ~a" op))
      (cond
        [(is-op? op #\1) (op12 + ints pos)]
        [(is-op? op #\2) (op12 * ints pos)]
        [(is-op? op #\3) (op3 ints pos system-id)]
        [(is-op? op #\4) (op4 ints pos)]
        [(is-op? op #\5) (jmp-op ints pos op5)]
        [(is-op? op #\6) (jmp-op ints pos op6)]
        [(is-op? op #\7) (cmp-op ints pos op7)]
        [(is-op? op #\8) (cmp-op ints pos op8)]
        [(string=? op "99") #f] ; Halt
        [else
         (displayln (format "Unknown opcode: ~a" (vector-ref ints pos)))]))))

(define (test input id)
  (displayln (format "Input: ~a, ID: ~a" input id))
  (set! system-id (format "~a" id))
  (intcode (input->vector input) 0))

; Part 1
;(intcode (input->vector input) 0)

; Part 2
(test input 5)

(define test2 "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")

;(test "3,9,8,9,10,9,4,9,99,-1,8" 7)
;(test "3,9,8,9,10,9,4,9,99,-1,8" 8)
;(test "3,9,8,9,10,9,4,9,99,-1,8" 9)

;(test test2 7)
;(test test2 8)
;(test test2 9)
