(define (square x) (* x x))

(define (cube x) (* x (square x)))

(define (improve guess x)
         (/ (+ (/ x (square guess))
               (* 2 guess))
            3))
 
(define (good-enough? guess x)
         (< (abs (- (cube guess) x))
            0.0001))

(define (cube-root-iter guess x)
         (if (good-enough? guess x)
             guess
             (cube-root-iter (improve guess x)
                        x)))

(define (round-decimal places num)
   (let ((x (expt 10.0 places)))
    (/ (round (* x num)) x)))

(define (cube-root x)
        (round-decimal 2 (cube-root-iter 1.00 x)))
