#lang racket

(require "input.rkt")

(define dbg #f)

(define (ddisplayln msg)
  (when dbg
    (displayln msg)))

(struct input-ele (num name) #:mutable #:transparent)
(struct element (name num inputs reqs remains) #:mutable #:transparent)

(define elements (make-hash))
(define ore-reqs (make-hash))

(define (parse-input input)
  (map (lambda (str)
         (define l (string-split str " "))
         (input-ele (string->number (first l)) (second l)))
       (map string-trim (string-split input ","))))

(define (parse-formula line)
  (define formula (map string-trim (string-split line "=>")))
  (define inputs (first formula))
  (define output (string-split (second formula) " "))
  (define ele (element (second output) (string->number (first output)) (parse-input inputs) 0 0)) 
  (hash-set! elements (element-name ele) ele))

(define (element-add-reqs name num remx)
  (ddisplayln (format "~a Add req: ~a, rem: ~a" name num remx))
  (let ([e (hash-ref elements name)])
    (cond
      [(>= remx num)
       (set-element-remains! e (- remx num))
       (ddisplayln (format "~a Act req: ~a, rem: ~a" name 0 (element-remains e)))
       0]
      [else
       (let* ([req (* (element-num e) (ceiling (/ (- num remx) (element-num e))))]
              [rem (if (<= req num)
                       (- (element-remains e) (- num req))
                       (+ (element-remains e) (- req num)))])
         (ddisplayln (format "~a Act req: ~a, rem: ~a" name req rem))
         (set-element-reqs! e (+ req (element-reqs e)))
         (set-element-remains! e rem)
         (ddisplayln (format "~a Cur req: ~a, rem: ~a" name
                             (element-reqs e) (element-remains e)))
         req)])))

(define (element-cal-inputs name num name-out)
  (ddisplayln (format ">> ~a" name-out))
  (let* ([e (hash-ref elements name)]
         [req (element-add-reqs name num (element-remains e))])
    (for-each
     (lambda (in)
       (let* ([req (* (input-ele-num in) (ceiling (/ req (element-num e))))]
              [rem (- req num)])
         (element-cal-inputs (input-ele-name in) req (format "~a->~a" name name-out))))
     (element-inputs e))))

(define (element-ore-input-num e)
  (define req 0)
  (for-each (lambda (in)
              (when (string=? "ORE" (input-ele-name in))
                (set! req (input-ele-num in))))
            (element-inputs e))
  req)

(define (ore-req-cal)
  (foldl + 0
         (hash-map ore-reqs
                   (lambda (k e)
                     (* (element-ore-input-num e)
                        (ceiling (/ (element-reqs e) (element-num e))))))))

(define (dump-element k v)
  (displayln (format "~a ~a, req: ~a = ~a"
                     (element-name v)
                     (element-num v)
                     (element-reqs v)
                     (element-inputs v))))

(define (nanofactory id)
  (define input (input-load-lines id))
  ; Parse formula
  (for-each (lambda (line)
              (ddisplayln line)
              (parse-formula line))
            input)
  (hash-set! elements "ORE" (element "ORE" 1  '() 0 0))

  (element-cal-inputs "FUEL" 1 "")
  (dump-element #f (hash-ref elements "ORE"))

  (define (part2)
    (let loop ([n 1])
      (let ([e (hash-ref elements "ORE")])
	(dump-element #f e)
	(cond
	  [(< (element-reqs e) 1000000000000) (loop (add1 n))]
	  [else
	    (displayln (format "It makes ~a FUELs" n))]))))
  
  ; Dump formulas
  ;(hash-for-each elements dump-element)
  (hash-for-each ore-reqs
                 (lambda (k e)
                   (displayln (format "~a reqs ~a ORE" k (element-reqs e)))))
  ;(ore-req-cal)
  )

(nanofactory "14")


