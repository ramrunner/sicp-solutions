;;EX 2.27 SICP
;;In this exercise we have to create a
;;deep reverse function
;;<dsp@2f30.org>
(define (dreverse l)
	(cond ((null? l) '())
				((pair? (car l))
				 (append (dreverse (cdr l))
								 (list (dreverse (car l)))))
				(else
					(append (dreverse (cdr l))
									(list (car l))))))

(display (dreverse (list (list 1 2) (list 3 4))))

;;EX 2.28 SICP
;;Define a fringe procedure that makes a 
;;tree left balanced.

(define (fringe l)
	(cond ((null? l) '())
				((pair? (car l))
				 (append (fringe (car l))
							   (fringe (cdr l))))
				(else
					l)))
				 
(newline)
(define x (list (list 1 2) (list 3 4)))
(display (fringe (list x x)))
