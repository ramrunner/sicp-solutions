;;EX 2.7 SICP
;;In this first part of an extended exercise will
;;create the representation and selectors of 
;;intervals, used for interval arithmetic
;;<dsp@2f30.org>

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (cond ((>= 0 (width-interval y))
	 (error "dividing by zero interval"))
	(else
	  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
					 (/ 1.0 (lower-bound y)))))))

(define (make-interval a b) (cons (min a b) (max a b)))

(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

;;Ex 2.8 SICP
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (upper-bound x) (upper-bound y))))

;;Ex 2.9 SICP

(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;Ex 2.10 SICP
;; 0 0 0 0 -> 1 -> same same (- -) -> 1 : h1*h2   l1*l2
;; 0 0 0 1 -> 2 -> same diff (- *) -> 6 : l1*h2   l1*l2
;; 0 0 1 0 -> 3 -> same diff (- *) -> 6 : l1*h2   l1*l2
;; 0 0 1 1 -> 4 -> same same (- +) -> 2 : l1*h2   h1*l2
;; 0 1 0 0 -> 5 -> diff same (* -) -> 7 : h1*h2   l1*l2
;; 0 1 0 1 -> 6 -> diff diff (* *) -> 5 : l1*l2   l1*h2    h1*l1   h1*h2
;; 0 1 1 0 -> 7 -> diff diff (* *) -> 5 : l1*l2   l1*h2    h1*l1   h1*h2
;; 0 1 1 1 -> 8 -> diff same (* +) -> 8 : l1*h2   h1*h2
;; 1 0 0 0 -> 9 -> diff same (* -) -> 7 : h1*h2   l1*l2
;; 1 0 0 1 -> 10-> diff diff (* *) -> 5 : l1*l2   l1*h2    h1*l1   h1*h2
;; 1 0 1 0 -> 11-> diff diff (* *) -> 5 : l1*l2   l1*h2    h1*l1   h1*h2
;; 1 0 1 1 -> 12-> diff same (* +) -> 8 : l1*h2   h1*h2
;; 1 1 0 0 -> 13-> same same (+ -) -> 3 : h1*l2   l1*h2
;; 1 1 0 1 -> 14-> same diff (+ *) -> 9 : h1*l2   h1*h2
;; 1 1 1 0 -> 15-> same diff (+ *) -> 9 : h1*l2   h1*h2
;; 1 1 1 1 -> 16-> same same (+ +) -> 4 : l1*l2   h1*h2

(define (mul-interval2 x y)
  (let* ((l1 (lower-bound x))
	 (l2 (lower-bound y))
	 (h1 (upper-bound x))
	 (h2 (upper-bound y))
	 (enf (lambda (a b c d)
		(cond ((and (< a 0) (< b 0) (< c 0) (< d 0))     1 )
		      ((and (< a 0) (< b 0) (< c 0) (>= d 0))    6 )
		      ((and (< a 0) (< b 0) (>= c 0) (< d 0))    6 )
		      ((and (< a 0) (< b 0) (>= c 0) (>= d 0))   2 )
		      ((and (< a 0) (>= b 0) (< c 0) (< d 0))    7 )
		      ((and (< a 0) (>= b 0) (< c 0) (>= d 0))   5 )
		      ((and (< a 0) (>= b 0) (>= c 0) (< d 0))   5 )
		      ((and (< a 0) (>= b 0) (>= c 0) (>= d 0))  8 )
		      ((and (>= a 0) (< b 0) (< c 0) (< d 0))    7 )
		      ((and (>= a 0) (< b 0) (< c 0) (>= d 0))   5 )
		      ((and (>= a 0) (< b 0) (>= c 0) (< d 0))   5 )
		      ((and (>= a 0) (< b 0) (>= c 0) (>= d 0))  8 )
		      ((and (>= a 0) (>= b 0) (< c 0) (< d 0))   3 )
		      ((and (>= a 0) (>= b 0) (< c 0) (>= d 0))  9 )
		      ((and (>= a 0) (>= b 0) (>= c 0) (< d 0))  9 )
		      ((and (>= a 0) (>= b 0) (>= c 0) (>= d 0)) 4 )
		      (else (error "mul-interval: enf : nonexistant condition")))))
	 (r (enf l1 h1 l2 h2)))
    (cond ((= r 1) (make-interval (* h1 h2) (* l1 l2)))
	  ((= r 2) (make-interval (* l1 h2) (* h1 l2)))
	  ((= r 3) (make-interval (* h1 l2) (* l1 h2)))
	  ((= r 4) (make-interval (* l1 l2) (* h1 h2)))
	  ((= r 5) (let ((p1 (* l1 l2))
			 (p2 (* l1 h2))
			 (p3 (* h1 l2))
			 (p4 (* h1 h2)))
		     (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
	  ((= r 6) (make-interval (* l1 h2) (* l1 l2)))
	  ((= r 7) (make-interval (* h1 h2) (* l1 l2)))
	  ((= r 8) (make-interval (* l1 h2) (* h1 h2)))
	  ((= r 9) (make-interval (* h1 l2) (* h1 h2))))))


(define (test29)
  (let* ((i1 (make-interval 2 5))
	 (i2 (make-interval 6 12))
	 (w1 (width-interval i1))
	 (w2 (width-interval i2))
	 (a1 (add-interval i1 i2))
	 (wa1 (width-interval a1))
	 (p1 (mul-interval i1 i2))
	 (wp1 (width-interval p1)))
    (display (list "adding interval" i1 " with width " w1))
    (newline)
    (display (list "adding interval" i2 " with width " w2))
    (newline)
    (display (list "res is " wa1))
    (newline)
    (display (list "prod is " p1 " and width " wp1))
    (newline)
    (display (div-interval (make-interval -2 3) (make-interval 2 2)))
    ))

(define (test210)
  (let ((i1 (make-interval -3 -1))
	(i2 (make-interval -2 3))
	(i3 (make-interval 12 17))
	(i4 (make-interval -1 9))
	(et? (lambda (i1 i2)
	       (and (= (upper-bound i1)
		       (upper-bound i2))
		    (= (lower-bound i1)
		       (lower-bound i2))))))
    (let ((m1 (mul-interval2 i1 i2))
	  (s1 (mul-interval i1 i2))
	  (m2 (mul-interval2 i1 i3))
	  (s2 (mul-interval i1 i3))
	  (m3 (mul-interval2 i1 i4))
	  (s3 (mul-interval i1 i4))
	  (m4 (mul-interval2 i2 i3))
	  (s4 (mul-interval i2 i3)))
      (cond ((and (et? m1 s1)
		  (et? m2 s2)
		  (et? m3 s3)
		  (et? m4 s4))
	     (display "Test passed!")(newline))
	    (else (display "test failed...") (newline))))
    (time (mul-interval2 i1 i2)
	  (mul-interval2 i1 i3)
	  (mul-interval2 i1 i3)
	  (mul-interval2 i1 i3)
	  (mul-interval2 i1 i2)
	  (mul-interval2 i2 i1)
	  (mul-interval2 i2 i3)
	  (mul-interval2 i2 i4)
	  (mul-interval2 i2 i3)
	  (mul-interval2 i2 i4)
	  (mul-interval2 i3 i4))
    (time (mul-interval i1 i2)
	  (mul-interval i1 i3)
	  (mul-interval i2 i3)
	  (mul-interval i1 i4)
	  (mul-interval i3 i3)
	  (mul-interval i2 i3)
	  (mul-interval i2 i1)
	  (mul-interval i2 i3)
	  (mul-interval i2 i3)
	  (mul-interval i2 i4)
	  (mul-interval i3 i4))))

;;EX 2.12 SICP
;;In this exercise we have to provide constructors for
;;intervals , that are defined using a center and a percentage
;;of error around it.
;; p= width/midpoint -> width=p*midpoint

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (* p c)) (+ c (* p c))))

(define (percent i)
  (/ (width-interval i) (center i)))


(define (test212)
  (let* ((i1 (make-center-percent 400 0.01))
	 (i2 (make-center-percent 20 0.02))
	 (p1 (mul-interval i1 i2)))
    (display (list i1 " , " i2 " , prod is: " p1))
    (newline)))
