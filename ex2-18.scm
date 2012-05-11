;;EX 2.18 SICP
;;Here we must create a procedure
;;that reverses a list
;;<dsp@2f30.org>

(define (reverse l)
	(letrec ((r (lambda (a b)
								(if (eq? a '())
									b
									(r (cdr a) (cons (car a) b))))))
		(r l '())))
