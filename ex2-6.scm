;;EX 2.6 SICP
;;In this exercise we must implement Church numerals
;;in specific, define one , two and the operation +
;;<dsp@2f30.org>

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
	(lambda (f) (lambda (x) (f ((n f) x)))))

(define one
	(add-1 zero))
;; one via substitution
;; (lambda (f) (lambda (x) (f ((zero f) x))))
;; (lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) y)) f) x))))
;; (lambda (f) (lambda (x) (f ((lambda (y) y) x))))
;; (lambda (f) (lambda (x) (f x)))
(define myone
  (lambda (f) (lambda (x) (f x))))

(define mytwo
	(lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
	(lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define (church2int n)
	(define  (inc x) (+ 1 x))
	((n inc) 0))

(define (test26)
	(cond ((= (church2int (add mytwo one)) 3)
				 (display "Test passed!")(newline))
				(else (error "Test Failed"))))
