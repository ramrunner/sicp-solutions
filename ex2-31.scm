;;EX 2.31 SICP
;;Here we have to finally create a tree-map ;)
;;<dsp@2f30.org>

(define (tree-map f tr)
	(map (lambda (st)
				 (if (pair? st)
					 (tree-map f st)
					 (f st)))
			 tr))

(define (square x)
	(* x x))

(define (test231)
	(let ((tt (list 1 2 (list 3 4 5 (list 6 7 8)) 9 10 11 (list 12 13))))
		(display (tree-map square tt))
		(newline)
		(display (tree-map cos tt))
		(newline)))
