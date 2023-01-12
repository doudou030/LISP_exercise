(define (variable? x)
  (symbol? x))

(define (same-variable? var1 var2)
  (and (variable? var1) (variable? var2) (eq? var1 var2)))

(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))

(define (product? p)
  (and (pair? p) (eq? (car p) '*)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (addend S) (cadr S))
(define (augend S) (caddr S))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
         (list '* m1 m2))))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))

(define (expleft E) (cadr E))
(define (expright E) (caddr E))

(define (make-exp u n)
  (cond ((=number? u 1) 1)
        ((=number? u 0) 0)
	((=number? n 0) 1)
	((=number? n 1) u)
        ((and (number? u) (number? n)) (expt u n))
        (else (list '** u n))))

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
        ((exponentiation? exp)
         (make-product (make-product 
	 (expright exp)(make-exp (expleft exp) (- (expright exp) 1)))
         (deriv (expleft exp) var)))))

