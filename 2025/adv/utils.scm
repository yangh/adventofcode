(define-module (adv utils)
	#:export (load-input pp p fold-add))

(use-modules (ice-9 pretty-print))
(use-modules (ice-9 textual-ports))
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

(define (load-input filename)
  (file->list filename))

(define (fold-add proc lst)
  (fold (lambda (ele prev)
          (+ prev (proc ele)))
        0 lst))
