;;EX 2.29 SICP
;;In this we have to create a procedure to see if a binary
;;mobile is balanced
;;<dsp@2f30.org>

(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cadr m))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cadr b))

(define (mob? x)
  (pair? (car x)))

(define (weig? x)
  (and (not (pair? (car x)))
       (number? (branch-structure x))))

(define (torque b)
  (cond ((weig? b)
	 (* (branch-length b)
	    (tweight b)))
	((mob? b)
	 (+ (torque (left-branch b))
	    (torque (right-branch b))))
	(else (* (branch-length b) (torque (branch-structure b))))))

(define (balanced? m)
  (= (torque (left-branch m))
     (torque (right-branch m))))



(define (tweight m)
  (cond ((null? m) 0)
	((mob? m)
	 (+ (tweight (left-branch m))
	    (tweight (right-branch m))))
	((weig? m)
	 (branch-structure m))
	(else
	  (tweight (branch-structure m)))))


(display (tweight (make-mobile (make-branch 5 10) (make-branch 12 (make-mobile (make-branch 20 12) (make-branch 8 9))))))
(newline)
(define tm (make-mobile (make-branch 2 4) (make-branch 2 4)))
(define tm1 (make-mobile (make-branch 2 (make-mobile (make-branch 1 2) (make-branch 100 3))) (make-branch 2 4)))
(display (torque tm))
(display (balanced? tm))
(newline)
(display (torque tm1))
(display (balanced? tm1))
(newline)
