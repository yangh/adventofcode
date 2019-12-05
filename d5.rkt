#lang racket

(define input "3,225,1,225,6,6,1100,1,238,225,104,0,2,106,196,224,101,-1157,224,224,4,224,102,8,223,223,1001,224,7,224,1,224,223,223,1002,144,30,224,1001,224,-1710,224,4,224,1002,223,8,223,101,1,224,224,1,224,223,223,101,82,109,224,1001,224,-111,224,4,224,102,8,223,223,1001,224,4,224,1,223,224,223,1102,10,50,225,1102,48,24,224,1001,224,-1152,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,1102,44,89,225,1101,29,74,225,1101,13,59,225,1101,49,60,225,1101,89,71,224,1001,224,-160,224,4,224,1002,223,8,223,1001,224,6,224,1,223,224,223,1101,27,57,225,102,23,114,224,1001,224,-1357,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,1001,192,49,224,1001,224,-121,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1102,81,72,225,1102,12,13,225,1,80,118,224,1001,224,-110,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,677,226,224,102,2,223,223,1005,224,329,101,1,223,223,108,226,226,224,102,2,223,223,1006,224,344,101,1,223,223,1108,226,677,224,102,2,223,223,1006,224,359,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,374,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,389,1001,223,1,223,107,677,226,224,1002,223,2,223,1005,224,404,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,419,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,434,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,449,1001,223,1,223,107,226,226,224,1002,223,2,223,1006,224,464,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,479,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,494,101,1,223,223,7,677,677,224,102,2,223,223,1005,224,509,1001,223,1,223,108,677,226,224,102,2,223,223,1005,224,524,101,1,223,223,1108,677,226,224,1002,223,2,223,1006,224,539,101,1,223,223,1108,677,677,224,102,2,223,223,1005,224,554,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,569,101,1,223,223,8,677,677,224,102,2,223,223,1005,224,584,101,1,223,223,1107,226,226,224,102,2,223,223,1006,224,599,101,1,223,223,108,677,677,224,102,2,223,223,1006,224,614,101,1,223,223,1008,677,226,224,1002,223,2,223,1005,224,629,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,644,101,1,223,223,1008,677,677,224,1002,223,2,223,1005,224,659,101,1,223,223,1007,677,226,224,1002,223,2,223,1005,224,674,1001,223,1,223,4,223,99,226")

(define (input->vector input)
  (list->vector
   (map string-trim
        (string-split input ","))))

;(displayln (format "Code length: ~a" (vector-length ins)))

(define (number-at ints pos)
  (string->number
   (vector-ref ints pos)))

(define (value-at ints pos)
  (number-at ints (number-at ints pos)))

(define (load-param ints op pos nth)
  (if (or
       ; No mode in opcode, Position mode as default
       (< (string-length op) (+ 2 nth))
       ; 0 - Possition mode
       (char=? #\0 (string-ref op (- (string-length op) nth 2))))
      (value-at ints (+ pos nth))
      ; 1 - Immediate mode
      (number-at ints (+ pos nth))))
      
(define (op12 func ints pos)
  (let* ([op (vector-ref ints pos)]
         [n1 (load-param ints op pos 1)]
         [n2 (load-param ints op pos 2)]
         [n3 (number-at ints (+ pos 3))])
    (displayln (format "OP: ~a ~a ~a ~a" op n1 n2 n3))
    (vector-set! ints n3 (format "~a" (func n1 n2)))))

(define (value-set! ints pos value)
  (displayln (format "Set: ~a ~a" pos value))
  (vector-set! ints pos value))

(define (op3 ints pos value)
  (value-set! ints (number-at ints (+ pos 1)) value))

(define (op4 ints pos)
  (let* ([op (vector-ref ints pos)]
         [n1 (load-param ints op pos 1)])
    (displayln (format "op4: ~a" n1))))

; jump-if-ture/false
(define (jmp-if cond ints pos)
  (let* ([op (vector-ref ints pos)]
         [n1 (load-param ints op pos 1)]
         [n2 (load-param ints op pos 2)])
    (displayln (format "jump-if: ~a ~a" n1 n2))
    (if (cond n1 0) n2 0)))

(define ne? (lambda (n1 n2) (not (= n1 n2))))

(define (op5 ints pos)
  (jmp-if ne? ints pos))

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

(define (op7 ints pos)
  (cmp-if < ints pos))

(define (op8 ints pos)
  (cmp-if = ints pos))

(define (cmp-op ints pos op)
  (let ([n3 (number-at ints (+ pos 3))]
        [value (if (op ints pos) "1" "0")])
    (value-set! ints n3 value)
    (intcode ints (+ pos 4))))

(define system-id "1")

(define (is-op? op code)
  (char=? (last (string->list op)) code))

(define (intcode ints pos)
  (when (< pos (vector-length ints))
    (let ([op (vector-ref ints pos)])
      ;(displayln (format "~a" ints))
      (displayln (format "pos: ~a" pos))
      (displayln (format "opc: ~a" op))
      (cond
        [(string=? op "99")
         ;(displayln ints)
         #f]
        [(is-op? op #\1)
         (op12 + ints pos)
         (intcode ints (+ pos 4))]
        [(is-op? op #\2)
         (op12 * ints pos)
         (intcode ints (+ pos 4))]
        [(string=? op "3")
         (op3 ints pos system-id)
         (intcode ints (+ pos 2))]
        [(is-op? op #\4)
         (op4 ints pos)
         (intcode ints (+ pos 2))]
        [(is-op? op #\5)
         (jmp-op ints pos op5)]
        [(is-op? op #\6)
         (jmp-op ints pos op6)]
        [(is-op? op #\7)
         (cmp-op ints pos op7)]
        [(is-op? op #\8)
         (cmp-op ints pos op8)]
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
