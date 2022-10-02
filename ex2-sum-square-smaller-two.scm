(define (square x) (* x x))

(define (sum-of-square-smaller-two a b c) (+ (square (min a b)) (square (min c (max a b)))))
