#lang racket

(struct opcode (name idx psize mcode))

(define Intcode%
  (class object%
    (super-new)

    ; Registers
    (define-values (intr int pc r1 r2 r3 r4 exp jmp) (values "" 0 0 0 0 0 0 #f #f))
    (define halt 99)
    (define codev #f)

    (define (goto p j) (set!-values (pc jmp) (values p j)))
    (define (jump p) (goto p #t))
    (define (move-pc psize)
      (when (not jmp) (goto (+ pc psize 1) #f)))

    (define (reset) (set!-values (intr int pc r1 r2 r3 r4 exp jmp) (values "" 0 0 0 0 0 0 #f #f)))
    (define (clear-flags) (set!-values (exp jmp) (values #f #f)))

    ; ABC[DE], DE is the code
    (define (load-intr)
      (let* ([code (vector-ref codev pc)]
             [len (string-length code)])
        (set! intr (string-append (make-string (- 5 len) #\0) code))
        (set! int (string->number (substring intr 3 5)))))
    
    ; Immediate mode parameter
    (define (number-at ints pos)
      (string->number (vector-ref ints pos)))

    ; Load parameters in 0/1 mode
    (define (load-parameter ints op pos nth)
      (if (char=? #\0 (string-ref op (- 3 nth)))
          (number-at ints (number-at ints (+ pos nth)))
          (number-at ints (+ pos nth))))

    (define (load-parameters opc psize)
      (when (> psize 0)
        (set! r1 (load-parameter codev intr pc 1)) ; At least 1 param
        (when (> psize 1)
          (set! r2 (load-parameter codev intr pc 2)))
        (when (> psize 2)
          (set! r3 (number-at codev (+ pc 3)))))) ; Result, always immediate

    (define (value-set! pos value)
      (vector-set! codev pos (format "~a" value)))

    (define user-input "1")
    (define/public (set-user-input n) (set! user-input (format "~a" n)))

    (define opcodev
      (list->vector
       (list ; Name Int Params Mcode
        (opcode "Hlt" 0 0 (lambda () (set! exp #t)))
        (opcode "Add" 1 3 (lambda () (value-set! r3 (+ r1 r2))))
        (opcode "Mul" 2 3 (lambda () (value-set! r3 (* r1 r2))))
        (opcode "Set" 3 1 (lambda () (value-set! (number-at codev (+ pc 1)) user-input)))
        (opcode "Out" 4 1 (lambda () (displayln (format "Output: ~a" r1))))
        (opcode "Jnz" 5 2 (lambda () (when (not (= r1 0)) (jump r2))))
        (opcode "Jz"  6 2 (lambda () (when (= r1 0) (jump r2))))
        (opcode "Lt"  7 3 (lambda () (value-set! r3 (if (< r1 r2) 1 0))))
        (opcode "Eq"  8 3 (lambda () (value-set! r3 (if (= r1 r2) 1 0)))))))

    (define/public (load-code input)
      (reset)
      (set! codev (list->vector (map string-trim (string-split input ",")))))

    (define (dump-cpu)
      ;(displayln codev)
      (displayln (format "regs: ~a/~a, ~a, ~a ~a ~a ~a, ~a ~a" intr int pc r1 r2 r3 r4 exp jmp)))

    (define/public (run)
      (clear-flags)
      (load-intr)
      (when (not (= int halt))
        (let* ([opc (vector-ref opcodev int)]
               [mcode (opcode-mcode opc)]
               [psize (opcode-psize opc)])
          (load-parameters opc psize)
          (dump-cpu)
          (mcode)
          (move-pc psize)
          (run))))))

(define ic (new Intcode%))

(send ic load-code "1,9,10,3,2,3,11,0,99,30,40,50")
(send ic run)

(define input "3,225,1,225,6,6,1100,1,238,225,104,0,2,106,196,224,101,-1157,224,224,4,224,102,8,223,223,1001,224,7,224,1,224,223,223,1002,144,30,224,1001,224,-1710,224,4,224,1002,223,8,223,101,1,224,224,1,224,223,223,101,82,109,224,1001,224,-111,224,4,224,102,8,223,223,1001,224,4,224,1,223,224,223,1102,10,50,225,1102,48,24,224,1001,224,-1152,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,1102,44,89,225,1101,29,74,225,1101,13,59,225,1101,49,60,225,1101,89,71,224,1001,224,-160,224,4,224,1002,223,8,223,1001,224,6,224,1,223,224,223,1101,27,57,225,102,23,114,224,1001,224,-1357,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,1001,192,49,224,1001,224,-121,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1102,81,72,225,1102,12,13,225,1,80,118,224,1001,224,-110,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,677,226,224,102,2,223,223,1005,224,329,101,1,223,223,108,226,226,224,102,2,223,223,1006,224,344,101,1,223,223,1108,226,677,224,102,2,223,223,1006,224,359,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,374,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,389,1001,223,1,223,107,677,226,224,1002,223,2,223,1005,224,404,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,419,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,434,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,449,1001,223,1,223,107,226,226,224,1002,223,2,223,1006,224,464,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,479,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,494,101,1,223,223,7,677,677,224,102,2,223,223,1005,224,509,1001,223,1,223,108,677,226,224,102,2,223,223,1005,224,524,101,1,223,223,1108,677,226,224,1002,223,2,223,1006,224,539,101,1,223,223,1108,677,677,224,102,2,223,223,1005,224,554,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,569,101,1,223,223,8,677,677,224,102,2,223,223,1005,224,584,101,1,223,223,1107,226,226,224,102,2,223,223,1006,224,599,101,1,223,223,108,677,677,224,102,2,223,223,1006,224,614,101,1,223,223,1008,677,226,224,1002,223,2,223,1005,224,629,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,644,101,1,223,223,1008,677,677,224,1002,223,2,223,1005,224,659,101,1,223,223,1007,677,226,224,1002,223,2,223,1005,224,674,1001,223,1,223,4,223,99,226")

; Part 1
(send ic load-code input)
(send ic set-user-input 1)
(send ic run)

; Part 2
(send ic load-code input)
(send ic set-user-input 5)
(send ic run)