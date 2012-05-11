;;Ex 2.17 SICP
;;We have to create a procedure last-pair
;;tha returns the last element of a non-empty
;;list
;;<dsp@2f30.org>

;(define (last-pair l)
;	(cond ((eq? l '())
;				 (error "last-pair : must give non-empty list"))
;				(else
;					(letrec ((iter (lambda (li place)
;						        (if (eq? li '())
;							        place
;							        (iter (cdr li) (car li))))))
;					   (iter l l)))))
(define (last-pair l)
	(letrec ((iter (lambda (l)
									 (if (eq? (cdr l) '())
										 l
										 (iter (cdr l))))))
		(car (iter l))))

(define (last-pair1 l)
	(cond ((eq? (cdr l) '())
				 (car l))
				(else (last-pair1 (cdr l)))))

(define (genrandlist n)
	(letrec ((iter (lambda (l n s)
									 (if (= n s)
										 l
										 (iter (cons (random n) l) n (+ s 1))))))
		(iter '() n 0)))
		

(define (test217)
	(let ((data (genrandlist 1800000)))
		(time (last-pair data))
		(time (last-pair1 data))))
