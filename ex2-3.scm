;;EX 2.3 SICP
;;We will create 2 different representations for rectangles.
;;The goal is to create 2 functions , one for the area and 
;;one for the perimeter that will work with both.
;;The trick will lie in the selectors of the rectangle class.
;;first repr will be the rectangle as a set of points (4 needed), 
;;the other as a set of segments (2 needed).
;;Of course the 2 representations are plain silly/wrong.
;;Using code from : Ex 2.2
;;providing a test function of no arguments.
;;<dsp@2f30.org>

(define make-point (lambda (x y)
  (cons x y)))

(define x-point (lambda (x)
  (car x)))

(define y-point (lambda (x)
  (cdr x)))

(define print-point (lambda (x)
  (display "(")
	(display (x-point x))
	(display ",")
	(display (y-point x))
	(display ")")
	(newline)))

(define make-segment (lambda (x y)
  (cons x y)))

(define start-segment (lambda (x)
  (car x)))

(define end-segment (lambda (x)
  (cdr x)))

(define midpoint-segment (lambda (x)
  (make-point (/ (- (x-point (end-segment x)) (x-point (start-segment x))) 2)
							(/ (- (y-point (end-segment x)) (y-point (start-segment x))) 2))))

(define length-segment (lambda (s)
  (let ((xs (x-point (start-segment s)))
				(ys (y-point (start-segment s)))
				(xe (x-point (end-segment s)))
				(ye (y-point (end-segment s))))
		(sqrt (+ (square (- xe xs)) (square (- ye ys)))))))

(define distance-points (lambda (p1 p2)
  (let ((x1 (x-point p1))
				(x2 (x-point p2))
				(y1 (y-point p1))
				(y2 (y-point p2)))
		(sqrt (+ (square (- x2 x1)) (square (- y2 y1)))))))

(define square (lambda (x)
  (* x x)))

(define make-rectangle1 (lambda (p1 p2 p3 p4)
  (list p1 p2 p3 p4)))

(define make-rectangle2 (lambda (s1 s2)
  (list s1 s2)))

;;this is the accessor that we care about repr
(define side-rectangle (lambda (r)
	(cond ((eq? (length r) 2)
				 (length-segment (car r)))
				((eq? (length r) 4)
				 (distance-points (car r) (cadr r)))
				(else (display "Error: side-rectangle. not a known representation")
							(newline)))))

(define perimeter-rectangle (lambda (r)
  (* 4 (side-rectangle r))))

(define area-rectangle (lambda (r)
  (square (side-rectangle r))))
  
(define test (lambda ()
  (let ((p1 (make-point 0 0))
        (p2 (make-point 0 10))
        (p3 (make-point 10 10))
        (p4 (make-point 10 0))
        (s1 (make-segment p1 p2))
        (s2 (make-segment p2 p3))
        (r1 (make-rectangle1 p1 p2 p3 p4))
        (r2 (make-rectangle2 s1 s2)))
		(display "repr1:: Area:")
		(display (area-rectangle r1))
		(display " perimeter:")
		(display (perimeter-rectangle r1))
		(newline)
		(display "repr2:: Area:")
		(display (area-rectangle r2))
		(display " perimeter:")
		(display (perimeter-rectangle r2))
		(newline)
		(cond ((and (= (area-rectangle r1)
									 (area-rectangle r2))
								(= (perimeter-rectangle r1)
                   (perimeter-rectangle r2)))
					 (display "all ok ! tests passed")
					 (newline))
					(else (display "tests failed"))))))

