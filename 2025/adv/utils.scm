(define-module (adv utils)
	#:export (load-input pp p
                       fold-add
                       fold-add-parallel
                       fold-append-parallel
                       dd tt adv1 adv2))

(use-modules (ice-9 pretty-print))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 threads))
(use-modules (ice-9 futures))
(use-modules (srfi srfi-1))

(define (pp obj)
  (pretty-print obj))

(define (p obj)
  (display obj) (newline))

(define (file->list filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((lines '()))
        (let ((line (get-line port)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines))))))))

;; Load file as list of each line
(define (load-input filename)
  (file->list filename))

;; Sum all the result from each element after call proc
(define (fold-add proc lst)
  (fold (lambda (ele prev)
          (+ prev (proc ele)))
        0 lst))

(use-modules (ice-9 futures) (ice-9 match))

(define (par-map proc lst)
  (match lst
    (()
     '())
    ((head tail ...)
     (let ((tail (future (par-map proc tail)))
           (head (proc head)))
       (cons head (touch tail))))))

;; Parallel fold add
(define (fold-add-parallel proc lst)
  (apply + (par-map proc lst)))
;; (fold + 0 (map touch (map future (map proc lst)))))

(define (fold-append-parallel proc lst)
  (apply append (par-map proc lst)))

(use-modules (ice-9 getopt-long))   ;; The official, most powerful parser

(define *option-spec*
  '((test     (single-char #\t) (value #f))        ;; -test or -t   → boolean
    (debug    (single-char #\d) (value #f))        ;; -debug or -d  → boolean
    (first    (single-char #\f) (value #f))        ;; -first X      → requires value
    (second   (single-char #\s) (value #f))        ;; -second Y     → requires value
    (verbose  (single-char #\v) (value #f))        ;; --verbose     → long option
    (help     (single-char #\h) (value #f))))      ;; -h / --help

;; -----------------------------------------------------------------------
;; 2. Parse argv
;; -----------------------------------------------------------------------
(define *options* (getopt-long (program-arguments) *option-spec*))
(define *opts*    (option-ref *options* '() '()))   ;; all parsed options

;; Helper to read an option (returns #f if not present)
(define (opt key) (option-ref *options* key #f))

;; -----------------------------------------------------------------------
;; 3. Extract values (with nice English names)
;; -----------------------------------------------------------------------
(define test?     (opt 'test))      ;; #t if -test or -t was given
(define debug?    (opt 'debug))     ;; #t if -debug or -d
;;(define first-val (opt 'first))     ;; string or #f
(define first?    (opt 'first))     ;; string or #f
(define second?   (opt 'second))   ;; string or #f
(define verbose?  (opt 'verbose))
(define help?     (opt 'help))

;; -----------------------------------------------------------------------
;; 4. Show help if requested
;; -----------------------------------------------------------------------
(when help?
  (display
   "Usage: my-program.scm [options] [files...]

Options:
  -t, --test              Enable test mode
  -d, --debug             Enable debug output
  -f, --first VALUE       First parameter
  -s, --second VALUE      Second parameter
  --verbose               Be verbose
  -h, --help              Show this help
")
  (exit 0))

(define (dd obj)
  (when debug? (pp obj)))

(define (tt obj)
  (when test? (pp obj)))

(define (adv1 obj)
  (when first? (pp obj)))

(define (adv2 obj)
  (when second? (pp obj)))

;; (use-modules (ice-9 curried-definitions))
;; (define-syntax-rule (dbg msg)
;;   (format #t "[DEBUG ~a] ~a~%" (or (current-procedure-name) 'anonymous) msg))
