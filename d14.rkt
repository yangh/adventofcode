#lang racket

(require "input.rkt")

(struct input-ele (num name) #:mutable)

(struct element (name num inputs reqs remains) #:mutable)

(define elehash (make-hash))
(define ore-reqs (make-hash))

(define (parse-input input)
  (map (lambda (str)
         (define l (string-split str " "))
         (input-ele (string->number (first l)) (second l)))
       (map string-trim (string-split input ","))))

(define (parse-formula line)
  (define f1 (map string-trim (string-split line "=>")))
  (define ins (first f1))
  (define outl (string-split (second f1) " "))
  (define ele (element (second outl) (string->number (first outl)) (parse-input ins) 0 0)) 
  (hash-set! elehash (element-name ele) ele))

(define (element-add-reqs name num remx)
  (displayln (format "~a Add req: ~a, rem: ~a" name num remx))
  (let* ([e (hash-ref elehash name)]
         [req (* (element-num e) (ceiling (/ num (element-num e))))]
         [rem (- req num)])
    (displayln (format "~a Act req: ~a, rem: ~a" name req rem))
    
    (cond
      [(>= (element-remains e) num)
       (set-element-remains! e (- (element-remains e) num))]
      [else
       (set-element-reqs! e (+ req (element-reqs e)))
       (set-element-remains! e (+ rem (element-remains e)))])
    (displayln (format "~a Cur req: ~a, rem: ~a" name
                       (element-reqs e) (element-remains e)))))

(define (element-cal-inputs name num rem name-out)
  (let* ([e (hash-ref elehash name)])
    (element-add-reqs name num rem)
    (for-each
     (lambda (in)
       (let* ([req (ceiling (/(* num (input-ele-num in))]
              [rem (- req num)])
         (element-cal-inputs
          (input-ele-name in)
          req rem name)))
     (element-inputs e))))

(define (element-ore-input-num e)
  (define req 0)
  (for-each (lambda (in)
              (when (string=? "ORE" (input-ele-name in))
                (set! req (input-ele-num in))))
            (element-inputs e))
  req)

(define (ore-req-cal)
  (foldl + 0 (hash-map ore-reqs
                       (lambda (k e)
                         (* (element-ore-input-num e)
                            (ceiling (/ (element-reqs e) (element-num e))))))))

(define (nanofactory id)
  (define input (input-load-lines id))
  ; Parse formula
  (for-each (lambda (line)
              (displayln line)
              (parse-formula line))
            input)
  (hash-set! elehash "ORE" (element "ORE" 1  '() 0 0))

  (element-cal-inputs "FUEL" 1 0 "")

  ; Dump formulas
  (hash-for-each elehash
                 (lambda (k v)
                   (displayln (format "~a ~a, req: ~a = ~a"
                                      (element-name v)
                                      (element-num v)
                                      (element-reqs v)
                                      (element-inputs v)))))
  (hash-for-each ore-reqs
                 (lambda (k e)
                   (displayln (format "~a reqs ~a ORE" k (element-reqs e)))))
  ;(ore-req-cal)
  )

(nanofactory "14-0")




