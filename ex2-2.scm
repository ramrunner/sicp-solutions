;;EX 2.2 SICP
;;We create constructors and accessors for line segments.
;;The segment is modeled as a start and an end point.
;;To do that first we create the same for a point in the plane
;;wich we model as a pair of numbers (x,y)
;;notice that the accessors for pairs are the same.
;;<dsp@2f30.org>

(define make-point 
  (lambda (x y)
    (cons x y)))

(define x-point 
  (lambda (x)
    (car x)))

(define y-point 
  (lambda (x)
    (cdr x)))

(define print-point 
  (lambda (x)
    (display "(")
    (display (x-point x))
    (display ",")
    (display (y-point x))
    (display ")")
    (newline)))

(define make-segment 
  (lambda (x y)
    (cons x y)))

(define start-segment 
  (lambda (x)
    (car x)))

(define end-segment 
  (lambda (x)
    (cdr x)))

(define midpoint-segment 
  (lambda (x)
    (make-point (/ (- (x-point (end-segment x)) (x-point (start-segment x))) 2)
		(/ (- (y-point (end-segment x)) (y-point (start-segment x))) 2))))
