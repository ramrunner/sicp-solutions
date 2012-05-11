;;EX 2.5 SICP
;;In this one we have to implement a procedural representation
;;that stores a pair of non-negative numbers a,b as the product
;;2^a*3^b, as well as their selectors.
;;<dsp@2f30.org>

(define (cons25 a b)
	(cond ((and (number? a)
							(number? b)
							(> a 0)
							(> b 0))
				 (* (expt 2 a) (expt 3 b)))
				(else (error "cons25 - only non-negative numbers"))))

(define  (divloop x y n)
	(cond ((= (remainder x y)  0)
				 (divloop (/ x y) y (+ n 1)))
				(else n)))

(define (car25 p)
	(divloop p 2 0))

(define (cdr25 p)
	(divloop p 3 0))
 
(define (test25)
	(cond ((and (= (car25 (cons25 3 4)) 3)
					    (= (cdr25 (cons25 3 4)) 4))
				 (display "Test 2.5 passed")(newline))
				(else (error "Test 2.5 failed"))))
