(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (- i 1)
	    (/ (n i)
	       (+ (d i)
		  result)))))
  (iter k 0.0))

;root f(y)= y^2-y-x
(define (root x)
  (+ 1.0 
     (cont-frac (lambda (y) x) (lambda (y) 1) 1000)
     )) 
