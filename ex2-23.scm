;;EX 2.23 SICP
;;implement a for-each procedure.
;;similar to map but just apply the
;;lambda on the arguments. don't for a list.
;;<dsp@2f30.org>

(define for-each1 
	(lambda (p l)
		(if (not (null? l))
			(begin 
				(p (car l))
				(for-each1 p (cdr l))))))

(for-each1 (lambda (x) (display x) (newline)) (list 1 2 52 42))
