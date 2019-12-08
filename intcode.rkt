#lang racket

; https://adventofcode.com/2019/day/2
; https://adventofcode.com/2019/day/5

(provide (all-defined-out))

(struct opcode (name psize mcode))

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
             [len  (string-length code)])
        (set! intr (string-append (make-string (- 5 len) #\0) code))
        (set! int  (string->number (substring intr 3 5)))))
    
    ; Immediate mode parameter
    (define (number-at pos)
      (string->number (vector-ref codev pos)))

    ; Load parameters in 0/1 mode
    (define (load-parameter nth)
      (if (char=? #\0 (string-ref intr (- 3 nth)))
          (number-at (number-at (+ pc nth)))
          (number-at (+ pc nth))))

    (define (load-parameters opc psize)
      (when (> psize 0)
        (set! r1 (load-parameter 1)) ; At least 1 param
        (when (> psize 1)
          (set! r2 (load-parameter 2)))
        (when (> psize 2)
          (set! r3 (number-at (+ pc 3)))))) ; Result, always immediate

    (define (value-set! pos value)
      (vector-set! codev pos (format "~a" value)))

    (define user-input "1")
    (define/public (set-user-input n) (set! user-input (format "~a" n)))

    (define opcodev
      (list->vector
       (list ; Name Int Params Mcode
        (opcode "Hlt" 0 (lambda () (set! exp #t)))
        (opcode "Add" 3 (lambda () (value-set! r3 (+ r1 r2))))
        (opcode "Mul" 3 (lambda () (value-set! r3 (* r1 r2))))
        (opcode "Set" 1 (lambda () (value-set! (number-at (+ pc 1)) user-input)))
        (opcode "Out" 1 (lambda () (displayln (format "Output: ~a" r1))))
        (opcode "Jnz" 2 (lambda () (when (not (= r1 0)) (jump r2))))
        (opcode "Jz"  2 (lambda () (when (= r1 0) (jump r2))))
        (opcode "Lt"  3 (lambda () (value-set! r3 (if (< r1 r2) 1 0))))
        (opcode "Eq"  3 (lambda () (value-set! r3 (if (= r1 r2) 1 0)))))))

    (define/public (load-code input)
      (reset)
      (set! codev (list->vector (map string-trim (string-split input ",")))))

    (define (dump-cpu)
      ;(displayln codev)
      (displayln (format "regs: ~a/~a, ~a, ~a ~a ~a ~a, ~a ~a" intr int pc r1 r2 r3 r4 exp jmp)))

    ; CPU core
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
