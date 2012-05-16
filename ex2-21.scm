;;EX 2.21 SICP
;;complete the square-list procedures
;;<dsp@2f30.org>

(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
    '()
    (cons (square (car items)) (square-list (cdr items)))))

(define (square-list1 items)
  (map square items))

(define (square-list2 items)
  (define (iter things ans)
    (if (null? things)
      ans
      (iter (cdr things)
	    (cons (square (car things))
		  ans))))
  (iter items '()))

(display (square-list '(1 2 4 5)))
(newline)
(display (square-list1 '(1 2 4 5)))
(newline)
(display (square-list2 '(1 2 4 5)))
(newline)
