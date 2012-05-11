 (require-extension xlib)
 ;(use  utils)
 (require-extension  easyffi)

;;components for the picture lang
(define (right-split painter n)
	(if (= n 0)
		painter
		(let ((smaller (right-split painter (- n 1))))
			(beside painter (below smaller smaller)))))

(define (corner-split painter n)
	(if (= n 0)
		painter
		(let ((up (up-split painter (- n 1)))
					(right (right-split painter (- n 1))))
			(let ((top-left (beside up up))
						(bottom-right (below right right))
						(corner (corner-split painter (- n 1))))
				(beside (below painter top-left)
								(below bottom-right corner))))))

(define (square-limit painter n)
	(let ((quarter (corner-split painter n)))
		(let ((half (beside (flip-horiz quarter) quarter)))
			(below (flip-vert half) half))))

(define (up-split painter n)
	(if (= n 0)
		painter
		(let ((smaller (up-split painter (- n 1))))
			(below painter (beside smaller smaller)))))

;;vectors
(define (make-v x y)
	(cons x y))

(define (x-v v)
	(car v))
(define (y-v v)
	(cdr v))
(define (add-v v1 v2)
	(cons (+ (x-v v1) (x-v v2)) (+ (y-v v1) (y-v v2))))
(define (sub-v v1 v2)
	(cons (- (x-v v1) (x-v v2)) (- (y-v v1) (y-v v2))))
(define (scl-v v1 n)
	(cons (* (x-v v1) n) (* (y-v v1) n)))

;;frames
(define (make-f o e1 e2)
	(list o e1 e2))
(define (orig-f f)
	(car f))
(define (e1-f f)
	(cadr f))
(define (e2-f f)
	(caddr f))
(define (frame-coord-map f)
	(lambda (v)
		(add-v (scl-v (x-v v)
									(e1-f f))
					 (scl-v (y-v v)
									(e2-f f)))))

;;combining painters
(define (transform-painter painter origin corn1 corn2)
	(lambda (frame)
		(let ((m (frame-coord-map frame)))
			(let ((new-origin (m origin)))
				(painter
					(make-frame new-origin
											(sub-v (m corn1) new-origin)
											(sub-v (m corn2) new-origin)))))))

(define (shrink-to-upper-right painter)
	(transform-painter painter
										 (make-vect 0.5 0.5)
										 (make-vect 1.0 0.5)
										 (make-vect 0.5 1.0)))
								#>	
							   typedef struct {
																short a1,a2,b1,b2;
																} XSegment;
                  XSegment *makesegs(int ox ,int oy ,int s1x ,int s1y, int s2x, int s2y){
									XSegment *s=(XSegment *) malloc(sizeof(XSegment *)*2);
									s[0].a1=ox;
									s[0].a2=oy;
									s[0].b1=ox+s1x;
									s[0].b2=ox+s1y;
									s[1].a1=ox;
									s[1].a2=oy;
									s[1].b1=ox+s2x;
									s[1].b2=oy+s2y;
									return s;
									}
							<#

(define kota display)
 
 (let ((display (xopendisplay #f)))
  (assert display)
  (let* ((screen (xdefaultscreen display))
 	(root (xrootwindow display screen))
 	(window (xcreatesimplewindow
 		 display root 100 200 300 50 0
 		 (xblackpixel display screen)
 		 (xwhitepixel display screen))))
   (assert window)
   (let ((font (xloadfont display "10x20")))
     (assert font)
     (let ((gc (xcreategc display window 0 #f))
 	     (event (make-xevent))
		   (depth (xdefaultdepth display screen)))
			 (let ((painter (lambda (f) 
              (xsetforeground display gc (xblackpixel display screen))
              (xsetbackground display gc (xwhitepixel display screen))
              (xsetfunction display gc GXCOPY)
              (xsetfont display gc font)
              (xselectinput display window (bitwise-ior EXPOSUREMASK BUTTONPRESSMASK))
              (xmapwindow display window)
              (xnextevent display event)
              (xdrawstring display window gc 100 30 "Paranoia is being computed..." 28)
							;(xdrawrectangle display window gc x y s1 s2)
						(define makesegs (foreign-lambda c-pointer "makesegs" int int int int int int))		
						  (kota f)
							(let* ((o (orig-f f))
										 (s1 (e1-f f))
										 (s2 (e2-f f))
										 (ox (x-v o))
										 (oy (y-v o))
										 (s1x (x-v s1))
										 (s1y (y-v s1))
										 (s2x (x-v s2))
										 (s2y (y-v s2)))
							  (xdrawsegments display window gc (makesegs ox oy s1x s1y s2x s2y) 2))
				;			(letrec ((leloop (lambda (num) 
				;											(cond ((< num 10)
				;			                  (xdrawarc display window gc x y (/ s1 num) (/ s2 num) 0 (/ 19000 num))
				;			                  ;(xdrawline display window gc (+ x (/ x num)) (+ y (/ y num)) (/ s1 2) (/ s2 2))
				;												(if (> num 2)
				;												(xdrawrectangle display window gc (+ x (/ s1 num)) (+ y (/ s2 num)) (/ s1 num ) (/ s2  num )))
				;												(leloop (+ num 1)))))))
				;				(leloop 1))
              (xflush display)
              (xnextevent display event))))
				 (let* ((v1 (make-v 30 50))
							  (v2 (make-v 122 544))
								(orig (make-v 355 199))
								(lef (make-f orig v1 v2)))
						 (painter lef) (system "sleep 10")))))))
