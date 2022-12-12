(define (my-cons a b)
    (lambda (f) (f a b)))
    
(define (my-car z)
  (z (lambda (p q) p)))

(define (my-cdr z)
  (z (lambda (p q) q)))
  
