#lang racket

; Used in https://adventofcode.com/2019/day/2,5,7,9,11,13,15,17

(provide (all-defined-out))

(define Intcode%
  (class object%
    (super-new)

    ; Debug
    (define debug #f)
    (define/public (set-debug n) (set! debug n))
    (define (debuginfo s) (when debug (displayln s)))
  
    ; Opcode: Name, Op Code, Param numbers, Micro code
    (struct opcode (name opc psize mcode))

    ; Instruction: parsed intr, opcode, parameters
    (struct instrc (intr opc p1 p2 p3))
    
    ; State
    (define-values (RESET RUNNING PAUSE IOWAIT HALT) (values 0 1  2 3 4))
    (define (state->string s)
      (define sstr (list 'RESET 'RUNNING 'PAUSE 'IOWAIT 'HALT))
      (if (< s (length sstr)) (list-ref sstr s) 'UNKNOWN))

    ; Registers
    (define-values (state intr pc r1 r2 r3 r4 rbase exp jmp)
      (values RESET #f 0 0 0 0 0 0 #f #f))

    ; Reset
    (define (reset)
      (set!-values (state intr pc r1 r2 r3 r4 rbase exp jmp)
                   (values RESET #f 0 0 0 0 0 0 #f #f))
      (codev-clear))

    (define (set-state s) (set! state s))
    (define (clear-flag-regs) (set!-values (exp jmp) (values #f #f)))
    (define (clear-general-regs) (set!-values (r1 r2 r3 r4) (values 0 0 0 0)))

    ; State API
    (define/public (is-halt?)   (= state HALT))
    (define/public (is-pause?)  (= state PAUSE))
    (define/public (is-iowait?) (= state IOWAIT))
    (define/public (wait-for-pause)
      (let loop ()
        (when (= state RUNNING)
          (debuginfo (format "Waiting for PAUSE...current: ~a" (state->string state)))
          ; TODO: use signal to save wait time
          (sleep 0.001)
          (loop))))

    ; CPU HALT, 99
    (define (cpu-halt)
      (set! exp #t)
      (set-state HALT))

    ; Update relative mode base
    (define (update-rbs offset)
      (debuginfo (format "Update Rbs: ~a + ~a = ~a" rbase offset (+ rbase offset)))
      (set! rbase (+ rbase offset)))

    ; Move PC, jump
    (define (goto p j) (set!-values (pc jmp) (values p j)))
    (define (jump p) (goto p #t))
    (define (move-pc psize)
      (when (not jmp) (goto (+ pc psize 1) #f)))

    ; Run until IOWAIT/PAUSE/HALT
    (define/public (run)
      (set-state RUNNING)
      (let loop ()
        (when (= state RUNNING)
          (cpu-run)
          (loop))))

    ; Computer memory, 1M numbers
    (define codev-size (* 1024 1024))
    (define codev (make-vector codev-size 0))
    (define (codev-clear) (vector-fill! codev 0))

    ; Instruction hash
    (define instr-cache (make-hasheqv))
    (define instr-cache-enabled #t) ; TBC: do we have any bug here?

    ; Load program into memory
    (define/public (load-code input)
      (reset)
      (let ([strs (string-split input ",")])
        (for ([idx (range 0 (length strs))])
          (vector-set! codev idx
                       (string->number(string-trim(list-ref strs idx)))))))

    ; Build new instr or query from hash
    (define (build-instr coden)
      (define halt 99)
      (define instr-len 5)

      (let* ([code (number->string coden)]
             [len  (string-length code)]
             [istr (string-append (make-string (- instr-len len) #\0) code)]
             [int (modulo coden 100)]) ; Last 2 numbers is int
        (instrc istr
                (if (= int halt) 0 int) ; HALT or other
                (string->number (substring istr 2 3))
                (string->number (substring istr 1 2))
                (string->number (substring istr 0 1)))))

    ; Parse instruction from number to struct instrc
    (define (parse-instr coden)
      (when instr-cache-enabled
        (when (not (hash-has-key? instr-cache coden))
          (hash-set! instr-cache coden (build-instr coden))))

      (if instr-cache-enabled
          (hash-ref instr-cache coden)
          (build-instr coden)))

    ; Load instruction into intr register from RAM at address in pc register
    ; ABC[DE], DE is the code
    (define (load-intr)
      (set! intr (parse-instr (vector-ref codev pc))))
    
    ; Load number from memory at pos
    (define (number-at pos) (vector-ref codev pos))

    ; Raw set on memory
    (define (value-set! pos value)
      (vector-set! codev pos value))

    ; Load parameters in 0/1/2 mode
    (define (load-parameter nth mode)
      (let ([num (number-at (+ pc nth))])
        (cond
          [(= mode 0) (number-at num)]           ; position mode
          [(= mode 1) num]                       ; immediate mode
          [(= mode 2) (number-at (+ rbase num))] ; relative mode
          [else (displayln ("ERROR: Unsupported parameter mode: ~a" mode))])))

    ; Support 0/1/2 mode only for first 2 parameters
    ; The output (3rd) parameter only support 1/2 mode
    (define (load-parameters psize)
      (when (> psize 0)
        (set! r1 (load-parameter 1 (instrc-p1 intr))) ; At least 1 param
        (when (> psize 1)
          (set! r2 (load-parameter 2 (instrc-p2 intr))))
        (when (> psize 2)
          (set! r3 (number-at (+ pc 3)))  ; Result, always immediate
          (when (= 2 (instrc-p3 intr))    ; Relative mode since day 9
            (set! r3 (+ rbase r3)))))
      ;(debuginfo (format "Load param for: ~a, ~a ~a ~a" (instrc-intr intr) r1 r2 r3))
      )

    ; Input, queued, FILO, not thread safely
    (define int-input '())

    (define/public (set-input n)
      (set! int-input (cons n int-input))
      (debuginfo (format "Add input: ~a, queue: ~a" n int-input))
      ; Mark state as ready to run
      (set! state RUNNING))

    (define/public (pop-input)
      (cond
        [(empty? int-input) #f] ; TODO: Use exception instead of #f
        [else
         (let ([n (car int-input)])
           (set! int-input (rest int-input))
           n)]))

    ; Output
    (define int-output 0)
    (define pause-on-output #f)
    (define/public (set-pause-on-output p) (set! pause-on-output p))

    (define/public (set-output n)
      (set! int-output n)
      (debuginfo (format "Set output: ~a" n))
      (when pause-on-output
        (set-state PAUSE)))

    (define/public (get-output) int-output)
    (define/public (display-output) (displayln (format "Output: ~a" int-output)))

    ; Load input, trigger IOWAIT if input queue is empty
    (define (load-input)
      (cond
        [(empty? int-input)
         (debuginfo "Need wait for input..")
         (set-state IOWAIT)]
        [else
         (let* ([addr  (number-at (+ pc 1))]
                [value (pop-input)]
                [mode  (instrc-p1 intr)]
                [dest  (if (= mode 2) (+ rbase addr) addr)]) ; Relative mode since day 9
           (value-set! dest value)
           (debuginfo (format "Load input: [~a] = ~a" dest value)))]))

    (define (cmpv func v1 v2 dst) (value-set! dst (if (func v1 v2) 1 0)))
    (define (jmp-if dst condition) (when condition (jump dst)))

    ; Micro code supported by the CPU
    (define opcodev
      (vector ;Name Int Params Micocode
       (opcode "Hlt" 0 0 (lambda () (cpu-halt)))
       (opcode "Add" 1 3 (lambda () (value-set! r3 (+ r1 r2))))
       (opcode "Mul" 2 3 (lambda () (value-set! r3 (* r1 r2))))
       (opcode "Set" 3 1 (lambda () (load-input)))
       (opcode "Out" 4 1 (lambda () (set-output r1)))
       (opcode "Jnz" 5 2 (lambda () (jmp-if r2 (not (= r1 0)))))
       (opcode "Jz"  6 2 (lambda () (jmp-if r2 (= r1 0))))
       (opcode "Lt"  7 3 (lambda () (cmpv < r1 r2 r3)))
       (opcode "Eq"  8 3 (lambda () (cmpv = r1 r2 r3)))
       (opcode "Rbs" 9 1 (lambda () (update-rbs r1)))))

    (define (dump-cpu)
      (debuginfo (format "regs: ~a, ~a, ~a ~a ~a ~a, ~a ~a"
                         (instrc-intr intr) pc r1 r2 r3 r4 exp jmp)))

    ; CPU ALU
    (define (cpu-run)
      (clear-flag-regs)
      (clear-general-regs)
      (load-intr)
      (let* ([opc (vector-ref opcodev (instrc-opc intr))]
             [mcode (opcode-mcode opc)]
             [psize (opcode-psize opc)])
        (load-parameters psize)
        ;(dump-cpu)
        (mcode)
        (when (or (= state RUNNING) ; IOWAIT will retry after resumed
                  (= state PAUSE))  ; PAUSE but will continue later
          (move-pc psize))
        ))
    ))
