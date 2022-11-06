(define (sum-divisors n)
 (+ n (sum-d n (- n 1))))

(define (sum-d n i)
 (cond ((= i 0) 0)
       ((= i 1) 1)
       ((= (remainder n i) 0) (+ i (sum-d n (- i 1))))
       (else (sum-d n (- i 1)))))

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
	  ((not (filter a)) (iter (next a) result))
	  (else (iter (next a) (combiner (term a) result)))))
  (iter a null-value))


