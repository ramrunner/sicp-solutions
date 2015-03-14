;;EX 2.56 SICP
;;we will extend the provided derivative program.
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;;The addend is the second item of the sum list
(define (addend s) (cadr s))

;;The augend is the third item of the sum list
(define (augend s) (caddr s))

;;A product is a list whose first element is the symbol *
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;;The multiplier is the second item of the product list
(define (multiplier p) (cadr p))

;;The multiplicand is the third item of the product list:
(define (multiplicand p) (caddr p))

;; start of exercise
;; the goal is to create primitives to handle exponentation
(define (exponentation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (make-exponentation e1 e2) 
  (cond ((= e2 0) 1)
        ((= e2 1) e1)
        (else (list '** e1 e2))))
