;;EX 2.30 SICP
;;In this exercise we have to implement a square-tree procedure
;;both directly and by using map
;;<dsp@2f30.org>

(define (sq-tr1 t)
	(cond ((null? t) '())
				((not (pair? t))
				 (* t t))
				(else
					(cons (sq-tr1 (car t))
								(sq-tr1 (cdr t))))))

(define (sq-tr2 t)
	(map (lambda (st)
				 (if (pair? st)
					 (sq-tr2 st)
					 (* st st)))
			 t))

(define (test230)
	(let ((tt (list 1 (list 2 3 4 (list 5 6)) 7 21 (list 10 10 10 10))))
		(display (sq-tr1 tt))
		(newline)
		(display (sq-tr2 tt))
		(newline)))

