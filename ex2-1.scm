;;EX 2.1 SICP 
;;In this one we construct a make-rat constructor  for rational numbers
;;that if both numbers are negative or both positive it returns the rat 
;; with positive terms, else the negative sign goes to the numerator
;;It also reduces the rat using the greatest common devisor , but this could
;;be done in the selectors numer and denom , if needed.
;;we also create accessors and a sample print for testing
;;<dsp@2f30.org>
(define make-rat (lambda (x y)
	(let ((g (gcd x y)))
	  (cond ((or  (and (< x 0) (< y 0)) (and (> x 0) (> y 0))) (cons (abs (/ x g)) (abs (/ y g))))
	  			((< y 0) (cons (- (/ x g)) (abs (/ y g))))
		 		  (else (cons (/ x g) (/ y g)))))))

(define numer (lambda (x)
	(car x)))

(define denom (lambda (x)
	(cdr x)))

(define print-rat (lambda (x)
	(display (numer x))
	(display "/")
	(display (denom x))
	(newline)))
