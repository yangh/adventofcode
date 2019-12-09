#lang racket

; https://adventofcode.com/2019/day/2
; https://adventofcode.com/2019/day/5

(provide (all-defined-out))

(struct opcode (name psize mcode))

(define Intcode%
  (class object%
    (super-new)
    
    (define debug #f)
    (define/public (set-debug n) (set! debug n))
    (define (debuginfo s)
      (when debug
        (displayln s)))

    ; Registers
    (define-values (intr int pc r1 r2 r3 r4 exp jmp iow hlt)
      (values "" 0 0 0 0 0 0 #f #f #f #f))

    ; CPU state
    (define-values (RESET RUNNING PAUSE IOWAIT HALT) (values 0 1  2 3 4))
    (define (state->string s)
      (define sstr (list 'RESET 'RUNNING 'PAUSE 'IOWAIT 'HALT))
      (list-ref sstr s))
    (define state RESET)
    (define (set-state s) (set! state s))
    (define/public (is-halt?) (= state HALT))
    (define/public (is-pause?) (= state PAUSE))
    (define/public (wait-for-pause)
      (let loop ()
        (when (= state RUNNING)
          (debuginfo (format "Waiting for PAUSE...current: ~a" (state->string state)))
          (sleep 0.1)
          (loop))))

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

    (define halt 99)
    (define codev #f)

    (define (goto p j) (set!-values (pc jmp) (values p j)))
    (define (jump p) (goto p #t))
    (define (move-pc psize)
      (when (not jmp) (goto (+ pc psize 1) #f)))

    (define (reset)
      (set!-values (intr int pc r1 r2 r3 r4 exp jmp iow hlt)
                   (values "" 0 0 0 0 0 0 #f #f #f #f))
      ;(set-state RESET)
      )
    (define (clear-flags) (set!-values (exp jmp iow hlt) (values #f #f #f #f)))
   
    ; ABC[DE], DE is the code
    (define (load-intr)
      (let* ([code (vector-ref codev pc)]
             [len  (string-length code)])
        (set! intr (string-append (make-string (- 5 len) #\0) code))
        (set! int  (string->number (substring intr 3 5)))
        ; HALT
        (when (= int 99)
          (set! int 0))))
    
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

    (define int-input '())
    (define/public (set-input n)
      (set! int-input (cons n int-input))
      (debuginfo (format "Add input: ~a, queue: ~a" n int-input))
      ;(thread-send cpu-thread 'i)
      )
    (define (pop-input)
      (cond
        [(empty? int-input) #f] ; TODO: Use exception instead of #f
        [else
         (let ([n (car int-input)])
           (set! int-input (rest int-input))
           n)]))

    (define int-output 0)
    (define/public (set-output n)
      (set! int-output n)
      (debuginfo (format "Set output: ~a" n))
      (when pause-on-output
        (set-state PAUSE))
      )
    (define/public (get-output) int-output)
    (define/public (display-output) (debuginfo (format "Output: ~a" int-output)))

    (define pause-on-output #f)
    (define/public (set-pause-on-output p) (set! pause-on-output p))
    
    (define (load-input)
      (let ([pos (number-at (+ pc 1))]
            [empty-input (empty? int-input)]
            [n (pop-input)])
        (cond
          [empty-input
           (debuginfo "Need wait for input..")
           (set-state IOWAIT)]
          [else
           (value-set! pos (number->string n))
           (debuginfo (format "Load input: [~a] = ~a" pos n))])
        ))

    (define (cpu-halt)
      (debuginfo "CPU HALT!")
      (set! exp #t)
      (set-state HALT))

    (define opcodev
      (list->vector
       (list ; Name Params Mcode
        (opcode "Hlt" 0 (lambda () (cpu-halt)))
        (opcode "Add" 3 (lambda () (value-set! r3 (+ r1 r2))))
        (opcode "Mul" 3 (lambda () (value-set! r3 (* r1 r2))))
        (opcode "Set" 1 (lambda () (load-input)))
        (opcode "Out" 1 (lambda () (set-output r1)))
        (opcode "Jnz" 2 (lambda () (when (not (= r1 0)) (jump r2))))
        (opcode "Jz"  2 (lambda () (when (= r1 0) (jump r2))))
        (opcode "Lt"  3 (lambda () (value-set! r3 (if (< r1 r2) 1 0))))
        (opcode "Eq"  3 (lambda () (value-set! r3 (if (= r1 r2) 1 0)))))))

    (define/public (load-code input)
      (reset)
      (set! codev (list->vector (map string-trim (string-split input ",")))))

    (define (dump-cpu)
      (debuginfo (format "regs: ~a/~a, ~a, ~a ~a ~a ~a, ~a ~a"
                         intr int pc r1 r2 r3 r4 exp jmp)))

    ; CPU core
    (define/public (cpu-run)
      (clear-flags)
      (load-intr)
      (let* ([opc (vector-ref opcodev int)]
             [mcode (opcode-mcode opc)]
             [psize (opcode-psize opc)])
        (load-parameters opc psize)
        ;(dump-cpu)
        (mcode)
        (when (= state RUNNING)
          (move-pc psize))
        ))

    (define/public (run)
      (set-state RUNNING)
      (thread-send cpu-thread 'r)
      (wait-for-pause))
    ))
