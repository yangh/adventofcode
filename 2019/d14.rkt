#lang racket

(require "input.rkt")

(define dbg #f)

(define (ddisplayln msg)
  (when dbg
    (displayln msg)))

(struct input-ele (num name) #:mutable #:transparent)
(struct element (name num inputs reqs remains) #:mutable #:transparent)

(define elements (make-hash))

(define (parse-input input)
  (map (lambda (str)
         (define l (string-split str " "))
         (input-ele (string->number (first l)) (second l)))
       (map string-trim (string-split input ","))))

(define (parse-formula line)
  (define formula (map string-trim (string-split line "=>")))
  (define inputs (first formula))
  (define output (string-split (second formula) " "))
  (let ([name (second output)])
    (hash-set! elements
               name
               (element (second output)
                        (string->number (first output))
                        (parse-input inputs)
                        0 0))))

(define (element-add-reqs name num rems)
  (ddisplayln (format "~a Add req: ~a, rem: ~a" name num rems))
  (let ([e (hash-ref elements name)])
    (cond
      ; Enough element in stock(remains)
      [(>= rems num)
       (set-element-remains! e (- rems num))
       (ddisplayln (format "~a Act req: ~a, rem: ~a" name 0 (element-remains e)))
       0]
      [else
       ; Allocate new element, use stock as much as possible
       (let* ([req (* (element-num e) (ceiling (/ (- num rems) (element-num e))))]
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
         (when (> req 0)
           (element-cal-inputs (input-ele-name in)
                               req
                               (format "~a->~a" name name-out)))))
     (element-inputs e))))

(define (dump-element k v)
  (displayln (format "~a ~a, req: ~a = ~a"
                     (element-name v)
                     (element-num v)
                     (element-reqs v)
                     (element-inputs v))))

(define (nanofactory input-id)
  ; Parse formula
  (for-each (lambda (line)
              (ddisplayln line)
              (parse-formula line))
            (input-load-lines input-id))

  ; Raw material
  (hash-set! elements "ORE" (element "ORE" 1  '() 0 0))

  (define (elements-reset)
    (hash-for-each elements
                   (lambda (k v)
                     (set-element-reqs! v 0)
                     (set-element-remains! v 0))))

  (define (ore-fuel-of n)
    (elements-reset)
    (element-cal-inputs "FUEL" n "")
    (element-reqs (hash-ref elements "ORE")))

  (define (part1)
    (displayln (format "1 FUEL requires ~a OREs" (ore-fuel-of 1))))

  (define (half a b) (inexact->exact (ceiling (/ (+ a b) 2))))

  (define (part2)
    (define ore-base (ore-fuel-of 1))
    (define ore-max 1000000000000)
    (define init-min (inexact->exact (ceiling (/ ore-max ore-base))))
    (define init-max (* 5 init-min))
    (ddisplayln (format "Search between: ~a ~ ~a, ore-base: ~a" init-min init-max ore-base))

    ; Binary search
    (let loop ([min init-min]
               [max init-max])
      (let* ([mid (half min max)]
             [ore (ore-fuel-of mid)])
        (ddisplayln (format "FUEL/ORE in half: ~a/~a/~a, ore: ~a; gap: ~a"
                            min mid max ore (- ore-max ore)))
        (cond
          [(> ore ore-max)
           (loop min (half mid max))]
          [(< (- ore-max ore) ore-base)
           (displayln (format "It makes ~a FUELs by ~a OREs" mid ore-max))]
          [else
           (loop (half min mid) max)]))))

  (part1)
  (part2))

(nanofactory "14")


