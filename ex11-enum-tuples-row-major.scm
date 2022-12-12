(define proc-list
  (lambda (lst F)
     (cond ((null? lst) '())
        (else (append (F (car lst)) (proc-list (cdr lst) F))))))

(define combine
  (lambda (L1 L2)
    (proc-list L1 (lambda (x)
                  (map (lambda (y) (list x y)) L2))))) 

(define (enum-tuples . L)
     (cond ((null? L) (append '()))
      	   ((null? (cdr L)) (map list (car L))) 
           ((null? (cddr L)) (combine (car L) (cadr L)))
           (else (proc-list (car L) (lambda (x) 
				    (map (lambda (y) (cons x y))
                                    (apply enum-tuples (cdr L)))))))) 
