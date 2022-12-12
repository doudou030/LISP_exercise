(define (preorder T)
    (cond ((null? T) T)
	  ((not (pair? T)) (list T)) 
	  (else (append (preorder (car T))
			(preorder (cdr T))))))





