;;EX 2.4 SICP
;;We have to verify a procedural representation
;;of the basic pair data structure.
;;goal is to implement cdr and verify that
;;(car (cons x y))->x for any x,y
;;<dsp@2f30.org>

(define (cons24 x y)
	(lambda (m) (m x y)))

(define (car24 z)
	(z (lambda (p q) p)))

(define (cdr24 z)
	(z (lambda (p q) q)))

(define (test-24)
	(let ((testx 'x)
				(testy 'y))
		(cond ((and (eq? (car24 (cons24 testx testy))
										 testx)
								(eq? (cdr24 (cons24 testx testy))
										 testy))
					 (display "Test passed")(newline))
					(else (error "Test failed")))))
