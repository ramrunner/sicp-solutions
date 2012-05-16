;;EX 2.32 SICP
;;In this one we have to finish
;;a precedure that generates all the possible
;;subsets of a set
;;<dsp@2f30.org>

(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (a)
			  (append (list (car s)) a))
			rest)))))

(display (subsets (list 1 2 3 4)))
