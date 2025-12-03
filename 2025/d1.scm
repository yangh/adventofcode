(use-modules (adv utils))
(use-modules (srfi srfi-1))

(define inputs (load-input "d1.txt"))

;; rround - count on over loop
(define (d1 rround)
  (let ((dial-n 50))
    (fold-add
     (lambda (ele)
       (dd ele)
       (let ((offset (modulo (string->number (substring ele 1)) 100))
             (rrs (floor-quotient (string->number (substring ele 1)) 100))
             (dir (string-ref ele 0))
             (dial-o dial-n))
         (let ((pwd (cond
                     ((char=? dir #\L)
                      (let ((ret (- dial-n offset)))
                        (cond
                         ((> ret 0) (set! dial-n ret) 0)
                         ((= ret 0) (set! dial-n ret) 1)
                         ((< ret 0) (set! dial-n (+ 100 ret))
                          (if (and rround (> dial-o 0)) 1 0)))))
                     ((char=? dir #\R)
                      (let ((ret (+ dial-n offset)))
                        (cond
                         ((< ret 100) (set! dial-n ret) 0)
                         ((= ret 100) (set! dial-n 0) 1)
                         ((> ret 100) (set! dial-n (- ret 100))
                          (if rround 1 0))))))))
           ;;(dd (list dial-o offset dial-n dir rrs pwd ele))
           (if rround
               (+ rrs pwd)
               pwd))))
     inputs)))

;; 1048
(pp (d1 #f))
;; 6498
(pp (d1 #t))
