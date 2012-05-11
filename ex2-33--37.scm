;;EX 2.33 SICP
;;Finish code for list-manipulations as
;;accumulations
;;<dsp@2f30.org>

(define (accumulate op init seq)
	(if (null? seq)
		init
		(op (car seq)
				(accumulate op init (cdr seq)))))

(define (tmap p sequence)
	(accumulate (lambda (x y)
								(if (not (list? x))
								(cons (p x)
											y)
								(cons (tmap p x)
											y)))
							'()
							sequence))

(define (append s1 s2)
	(accumulate cons s1 s2))

(define (len seq)
	(accumulate (lambda (x y) (if (not (null? x)) (+ 1 y)))
							0
							seq))

(define (square x)
	(* x x))

(define (test233)
	(let ((tt (list 1 2 (list 3 4 5 (list 6 7) 8) 9 10 (list 11)))
				(tt1 (list 737737 73737)))
		(display (tmap square tt))
		(newline)
		(len tt)
		(newline)
		(display (append tt tt1))
		(newline)))

;;EX 2.34 SICP 
;;create a horner eval for polynomials
(define (horner-eval x highcoeffs)
	(accumulate (lambda (thiscoeff highterms)
								(if (not (null? highterms))
								(+ thiscoeff
									 (* x
									    highterms))))
							0
							highcoeffs))

(define (test234)
	(display "coeff 1+3x+5x^3+x^5 at x=2")
	(newline)
	(display (horner-eval 2 (list 1 3 0 5 0 1)))
	(newline))


;;EX 2.35 SICP
;;redefine count-leaves as an accumulation
;; i really can't find any meaning to that map
;; so i "got around" it using the identity.
;; possibly a fringe+len would be good solution too.

(define (count-leaves t)
	(accumulate (lambda (a b)
								(if (list? a)
									(+ b (count-leaves a))
									(+ b 1)))
							0
							(map (lambda (x) x) t)))

(define (test235)
	(let ((tt (list 1 2 (list 3 4 5) (list 6 7) 8 9 (list 10 11 12))))
		(display (count-leaves tt))
		(newline)))


;;EX 2.36 SICP
;;accumulate-n compines same-pos elems of different same-lenth lists
;;to produce a list of results.

(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		'()
		(cons (accumulate op init (map car seqs))
					(accumulate-n op init (map cdr seqs)))))

(define (test236)
	(let ((ts (list (list 1 2 3) (list 2 3 4) (list 3 4 5))))
		(display (accumulate-n + 0 ts))
		(newline)))

;;EX 2.37
;;using accumulate implement matrix-vector arithmetic

(define (dot-product v w)
	(accumulate + 0 (map * v w)))

(define (mat*vec m v)
	(map (lambda (x) (accumulate + 0 (map * x v))) m))

(define (transpose mat)
	(accumulate-n (lambda (a b)
									(cons a b))
								'()
								mat))

(define (mat*mat m1 m2)
	(let ((cols (transpose m2)))
		(map (lambda (a)
					 (mat*vec cols a))
					  m1)))


(define (test237)
	(let ((tm (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
				(tm1 (list (list 1 2 3) (list 3 5 1) (list 1 1 1)))
				(tv (list 1 1 1)))
	;	(display (dot-product tv tv))
;		(newline)
		(display (transpose tm))
		(newline)
		(display (mat*mat tm1 tm))
		(newline)
		(display (mat*vec tm tv))
		(newline)))



