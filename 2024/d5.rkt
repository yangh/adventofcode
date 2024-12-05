#lang racket

(require math/base)
(require "utils.rkt")

(define lines (input-load-lines "5"))

(define print-rules (make-vector 100 '()))
(define num-rules 0)

(for ([line lines]) #:break (= 0 (string-length line))
  ;(displayln line)
  (set! num-rules (add1 num-rules))
  (let* ([strs (string-split line "|")]
         [id1 (string->number (first strs))]
         [id2 (string->number (second strs))])
    (vector-set! print-rules id1
                 (append (vector-ref print-rules id1) (list id2)))))

(define new-pages
  (map (lambda (line)
         (map string->number (string-split line ",")))
       (take-right lines (- (length lines) num-rules 1))))

(define (is-in-list? v l)
  (if (member v l)
      #t  ; Return true if v is found in the list
      #f)) ; Return false if v is not found

(define (valid-group v1 v2)
  (let ([rules (vector-ref print-rules v1)])
    (and (not (empty? rules))
         (is-in-list? v2 rules))))

;(valid-group 75 47)

(define (valid-pages? pages)
  (or (= 1 (length pages)) ; last one
      (and
        (let ([v1 (first pages)])
          (andmap (lambda (v2) (valid-group v1 v2)) (rest pages)))
        (valid-pages? (rest pages)))))

;(valid-pages (first new-pages))

(define (num-in-middle pages)
  (list-ref pages (floor (/ (length pages) 2))))

(define valid-pages
  (filter valid-pages? new-pages))

(define incorrect-pages
  (filter (lambda (pages) (not (valid-pages? pages))) new-pages))

; 4462
(sum (map num-in-middle valid-pages))

(define (pages-add page pages)
  (let ([len (length pages)])
    (let loop ([pos 0])
      (let ([new-pages (append (take pages pos) (list page) (take-right pages (- len pos)))])
        (cond ([valid-pages? new-pages] new-pages)
              (else (loop (add1 pos))))))))

;(pages-add 97 '(75))
;(pages-add 47 '(97 75))

(define corrected-pages
  (map (lambda (pages)
	 (foldl pages-add '() pages))
               incorrect-pages))

; 6767
(sum (map num-in-middle corrected-pages))
