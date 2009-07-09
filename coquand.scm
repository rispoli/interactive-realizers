(define phi
  (lambda (x)
    (* (+ x 1) (- x 2) (/ (- x 4) 2))))

(define a 1)

(define m
  (lambda (s)
    ; mu <state> <initial_value>
    (mu s 1)))

(define P
  (lambda (n m)
    (> (phi n) (phi m))))

(define F
  (lambda (s)
    (list
      (lambda (s) (list P (m s) (+ (m s) a))))))
