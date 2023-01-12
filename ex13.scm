
(define (make-monitored f)
  (let ((count 0))
    (lambda (arg)
      (set! count (+ count 1))
      (f arg))))

(define s (make-monitored sqrt))
(output (s 100))


(define (make-monitored f)
  (let ((count 0))
    ; message dispatcher procedure
    (lambda (m)
      (cond ((equal? m 'how-many-calls?) count)
            ((equal? m 'reset-count) (set! count 0))
            (else 
              (begin
                (set! count (+ count 1))
                (f m)))))))

