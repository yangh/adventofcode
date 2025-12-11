(define-module (adv utils)
	#:export (load-input pp p
                       fold-add
                       fold-add-parallel
                       fold-append
                       fold-append-parallel
                       fold-append-uniq
                       dd tt adv1 adv2
                       add1 sub1
                       list-findp
                       list-uniq
                       list-uniq-add
                       list-uniq-append
                       list-head1
                       list-tail1
                       ))

(use-modules (ice-9 pretty-print))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 threads))
(use-modules (ice-9 futures))
(use-modules (srfi srfi-1))

(define (pp obj)
  (pretty-print obj
                #:width 100
                #:max-expr-width 80
                ))

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
  (file->list (string-append
               "inputs/"
               filename
               (if example? ".0" "")
               ".txt")))

;; Sum all the result from each element after call proc
(define (fold-add proc lst)
  (fold (lambda (ele prev)
          (+ prev (proc ele)))
        0 lst))

(define (fold-append proc lst)
  (fold (lambda (ele prev)
          (append prev (proc ele)))
        '() lst))

(define (fold-append-uniq proc lst)
  (fold (lambda (ele prev)
          (list-uniq-append prev (proc ele)))
        '() lst))

;; Parallel fold add
(define (fold-add-parallel proc lst)
  (apply + (par-map proc lst)))
;; (fold + 0 (map touch (map future (map proc lst)))))

(define (fold-append-parallel proc lst)
  (apply append (par-map proc lst)))

(define (add1 v) (+ v 1))
(define (sub1 v) (- v 1))

(use-modules (ice-9 getopt-long))   ;; The official, most powerful parser

(define *option-spec*
  '((test     (single-char #\t) (value #f))        ;; -test or -t   → boolean
    (debug    (single-char #\d) (value #f))        ;; -debug or -d  → boolean
    (example  (single-char #\e) (value #f))        ;; -example or -e  → boolean
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
(define example?  (opt 'example))   ;; #t if -example or -e
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
  -e, --example           Use example input
  --verbose               Be verbose
  -h, --help              Show this help
")
  (exit 0))

(define (ddd obj)
  (when debug? (pp obj)))

(define-syntax dd
  (syntax-rules ()
    ((_ expr ...)
     (when debug?
       ;;(display "DEBUG [")
       ;;(display (current-procedure-name))
       ;;(display "] ")
       (for-each
        (lambda (name value)
          (format #t "~a =\n" name)
          (pp value))
        '(expr ...)
        (list expr ...))
       (newline)))))

(define (tt obj)
  (when test? (pp obj)))

(define (adv1 obj)
  (when first? (pp obj)))

(define (adv2 obj)
  (when second? (pp obj)))

;; (use-modules (ice-9 curried-definitions))
;; (define-syntax-rule (dbg msg)
;;   (format #t "[DEBUG ~a] ~a~%" (or (current-procedure-name) 'anonymous) msg))

;; List utils
(define (list-findp lst v)
  (let find ((p 0) (lst lst))
    (cond
     ((null? lst) #f)
     ((eq? (car lst) v) p)
     (else (find (add1 p) (cdr lst))))))

(define (list-uniq-add lst v)
  (if (list-findp lst v)
      lst
      (append lst (list v))))

(define (list-uniq lst)
  (let loop ((ret '()) (lst lst))
    ;;(pp (list "list-uniq" ret lst))
    (if (null? lst) ret
        (loop (list-uniq-add ret (car lst))
              (cdr lst)))))

;;(tt (list "Uniq" (list-uniq '(3 4 5 3 4 5 1 1 1))))

(define (list-uniq-append prev lst)
  (let loop ((ret prev) (lst lst))
    ;;(pp (list "list-uniq" ret lst))
    (if (null? lst) ret
        (loop (list-uniq-add ret (car lst))
              (cdr lst)))))

(define (list-head1 lst)
  (and lst
       (not (null? lst))
       (car lst)))

(define (list-tail1 lst)
  (and lst
       (not (null? lst))
       (list-ref lst (sub1 (length lst)))))
