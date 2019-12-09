#lang racket

; https://adventofcode.com/2019/day/2
; https://adventofcode.com/2019/day/5

(provide (all-defined-out))

(struct opcode (name int psize mcode))

(define Intcode%
  (class object%
    (super-new)

    ; Debug
    (define debug #f)
    (define/public (set-debug n) (set! debug n))
    (define (debuginfo s) (when debug (displayln s)))

    ; State
    (define-values (RESET RUNNING PAUSE IOWAIT HALT) (values 0 1  2 3 4))
    (define (state->string s)
      (define sstr (list 'RESET 'RUNNING 'PAUSE 'IOWAIT 'HALT))
      (list-ref sstr s))

    ; Registers
    (define-values (state intr int pc r1 r2 r3 r4 rbs exp jmp)
      (values RESET "" 0 0 0 0 0 0 0 #f #f))

    (define (reset)
      (set!-values (state intr int pc r1 r2 r3 r4 rbs exp jmp)
                   (values RESET "" 0 0 0 0 0 0 0 #f #f)))

    (define (set-state s) (set! state s))
    (define (clear-flags) (set!-values (exp jmp) (values #f #f)))
    (define (clear-tmp-regs)
      (set!-values (r1 r2 r3 r4)
                   (values 0 0 0 0)))

    (define/public (is-halt?) (= state HALT))
    (define/public (is-pause?) (= state PAUSE))
    (define/public (wait-for-pause)
      (let loop ()
        (when (= state RUNNING)
          (debuginfo (format "Waiting for PAUSE...current: ~a" (state->string state)))
          ; TODO: use signal to save wait time
          (sleep 0.1)
          (loop))))

    ; CPU HALT, 99
    (define (cpu-halt)
      (set! exp #t)
      (set-state HALT))

    ; Update relative mode base
    (define (update-rbs offset)
      (debuginfo (format "Update Rbs: ~a + ~a = ~a" rbs offset (+ rbs offset)))
      (set! rbs (+ rbs offset)))

    ; Move PC, jump
    (define (goto p j) (set!-values (pc jmp) (values p j)))
    (define (jump p) (goto p #t))
    (define (move-pc psize)
      (when (not jmp) (goto (+ pc psize 1) #f)))

    ; CPU core thread
    (define cpu-thread
      (thread
       (lambda ()
         (let loop ()
           (cond
             [(= state RUNNING)
              (cpu-run)]
             [else
              ; Wait some event before continue
              (debuginfo (format "CPU paused due to: ~a" (state->string state)))
              (thread-receive)
              (set-state RUNNING)
              (debuginfo "Start to RUN")])
           (loop)))))

    ; Run until IOWAIT/PAUSE/HALT
    (define/public (run)
      (set-state RUNNING)
      (thread-send cpu-thread 'r)
      (wait-for-pause))

    ; Computer memory, 1M numbers
    (define codev (make-vector (* 1024 1024) "0"))

    ; Load program into memory
    (define/public (load-code input)
      (reset)
      (let ([strs (map string-trim (string-split input ","))])
        (for ([idx (range 0 (length strs))])
          (vector-set! codev idx (list-ref strs idx)))))

    (define halt 99)

    ; ABC[DE], DE is the code
    (define (load-intr)
      (let* ([code (vector-ref codev pc)]
             [len  (string-length code)])
        (set! intr (string-append (make-string (- 5 len) #\0) code))
        (set! int  (string->number (substring intr 3 5)))
        ; HALT
        (when (= int halt) (set! int 0))))
    
    ; Load number from memory at pos
    (define (number-at pos)
      (let ([str (vector-ref codev pos)])
        ;(debuginfo (format "Load number[~a] = ~a" pos str))
        (string->number str)))

    ; Raw set on memory
    (define (value-set! pos value)
      (vector-set! codev pos (format "~a" value)))

    ; Load parameters in 0/1/2 mode
    (define (load-parameter nth)
      (let ([mode (string-ref intr (- 3 nth))])
        (cond
          [(char=? #\0 mode) ; position mode
           (number-at (number-at (+ pc nth)))]
          [(char=? #\1 mode) ; immediate mode
           (number-at (+ pc nth))]
          [(char=? #\2 mode) ; relative mode
           (number-at (+ rbs (number-at (+ pc nth))))]
          [else
           (displayln ("Unsupported parameter mode: ~a" mode))])))

    ; Support 0/1/2 mode only for first 2 parameters
    ; The output (3rd) parameter only support 1/2 mode
    (define (load-parameters psize)
      (when (> psize 0)
        (set! r1 (load-parameter 1)) ; At least 1 param
        (when (> psize 1)
          (set! r2 (load-parameter 2)))
        (when (> psize 2)
          (set! r3 (number-at (+ pc 3)))  ; Result, always immediate
          (when (char=? #\2 (string-ref intr 0)) ; Relative mode since day 9
            (set! r3 (+ rbs r3)))))
      ;(debuginfo (format "Load param for: ~a, ~a ~a ~a" intr r1 r2 r3))
      )

    ; Input, queued, FILO, not thread safely
    (define int-input '())
    (define/public (set-input n)
      (set! int-input (cons n int-input))
      (debuginfo (format "Add input: ~a, queue: ~a" n int-input)))

    (define (pop-input)
      (cond
        [(empty? int-input) #f] ; TODO: Use exception instead of #f
        [else
         (let ([n (car int-input)])
           (set! int-input (rest int-input))
           n)]))

    ; Output
    (define int-output 0)
    (define/public (set-output n)
      (set! int-output n)
      (debuginfo (format "Set output: ~a" n))
      (when pause-on-output
        (set-state PAUSE)))
    (define/public (get-output) int-output)
    (define/public (display-output) (displayln (format "Output: ~a" int-output)))

    (define pause-on-output #f)
    (define/public (set-pause-on-output p) (set! pause-on-output p))

    ; Load input, trigger IOWAIT if input queue is empty
    (define (load-input)
      (cond
        [(empty? int-input)
         (debuginfo "Need wait for input..")
         (set-state IOWAIT)]
        [else
         (let* ([p1 (number-at (+ pc 1))]
                [n (pop-input)]
                [mode (string-ref intr 2)]
                [pos (if (char=? mode #\2) (+ rbs p1) p1)]) ; Relative mode since day 9
           (value-set! pos (number->string n))
           (debuginfo (format "Load input: [~a] = ~a" pos n)))]))

    ; Micro code supported by the CPU
    (define opcodev
      (list->vector
       (list ; Name Int Params Mcode
        (opcode "Hlt" 0 0 (lambda () (cpu-halt)))
        (opcode "Add" 1 3 (lambda () (value-set! r3 (+ r1 r2))))
        (opcode "Mul" 2 3 (lambda () (value-set! r3 (* r1 r2))))
        (opcode "Set" 3 1 (lambda () (load-input)))
        (opcode "Out" 4 1 (lambda () (set-output r1)))
        (opcode "Jnz" 5 2 (lambda () (when (not (= r1 0)) (jump r2))))
        (opcode "Jz"  6 2 (lambda () (when (= r1 0) (jump r2))))
        (opcode "Lt"  7 3 (lambda () (value-set! r3 (if (< r1 r2) 1 0))))
        (opcode "Eq"  8 3 (lambda () (value-set! r3 (if (= r1 r2) 1 0))))
        (opcode "Rbs" 9 1 (lambda () (update-rbs r1)))
        )))

    (define (dump-cpu)
      (debuginfo (format "regs: ~a/~a, ~a, ~a ~a ~a ~a, ~a ~a"
                         intr int pc r1 r2 r3 r4 exp jmp)))

    ; CPU ALU
    (define (cpu-run)
      (clear-flags)
      (clear-tmp-regs)
      (load-intr)
      (let* ([opc (vector-ref opcodev int)]
             [mcode (opcode-mcode opc)]
             [psize (opcode-psize opc)])
        (load-parameters psize)
        ;(dump-cpu)
        (mcode)
        (when (= state RUNNING) ; IOWAIT will retry after resumed
          (move-pc psize))
        ))
    ))
