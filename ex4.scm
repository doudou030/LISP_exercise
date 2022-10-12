(define (square x) (* x x))

(define (mod a) (modulo a 3))

(define (fun-rec n)
  (if (< n 3) 
       (+ (* 3 (square n)) (* -7 n) 7)
       (cond ((= (mod n) 0) (+ (* -1 (fun-rec (- n 1))) (fun-rec (- n 2)) (fun-rec (- n 3))))
             ((= (mod n) 1) (+ (fun-rec (- n 1)) (* -1 (fun-rec (- n 2))) (fun-rec (- n 3))))
             ((= (mod n) 2) (+ (* -1 (fun-rec (- n 1))) (fun-rec (- n 2)) (* -1 (fun-rec (- n 3))))))))


(define (fun-ite n) 
   (define (iter x y z count)
      (cond ((= n 0) 7)
            ((= n 1) 3)
            ((= n 2) 5)
            ((> count n) x)
            ((and (> n 2) (= (mod count) 0)) (iter (+ (- y x) z) x y (+ count 1)))
            ((and (> n 2) (= (mod count) 1)) (iter (+ (- x y) z) x y (+ count 1)))
            ((and (> n 2) (= (mod count) 2)) (iter (- (- y x) z) x y (+ count 1)))))
   (iter 5 3 7 3)) 

