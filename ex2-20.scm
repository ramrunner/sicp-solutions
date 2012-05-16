;;EX 2.20
;;create a function same parity
;;that accepts >1 arguments
;;and it returns the elements with the same
;;parity as the first term
;;<dsp@2f30.org>

(define (same-parity a . b)
  (letrec  ((par? (if (even? a)
		    even?
		    odd?))
	    (res (list a))
	    (iter (lambda (l r)
		    (if (eq? l '())
		      r
		      (begin
			(let ((a (car l)))
			  (if (par? a)
			    (iter (cdr l) (append r a))
			    (iter (cdr l) r))))))))
    (iter b res)))

(display (same-parity 1 2 3 4 5 6 7 8 11 20 242 243))
(newline)
