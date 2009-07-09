(define f
  (lambda (n)
    (+ (* (+ n 1) (- n 2) (/ (- n 4) 2)) 2)))

(define g
  (lambda (n)
    (+ n 1)))

(define h
  (lambda (n)
    (+ (* (+ n 7) (/ (- n 4) 2)) 5)))

(define m
  (lambda (s)
    ; mu <state> <initial_value>
    (mu s 1)))

(define P
  (lambda (n m)
    (> (f n) (f m))))

(define F
  (lambda (s)
    (list
      (lambda (s) (list P (m s) (g (m s))))
      (lambda (s) (list P (m s) (h (m s)))))))
