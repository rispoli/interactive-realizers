(load "find-state-with-lists.scm")

;;; Esempio Coquand:

(define phi
  (lambda (x)
    (* (+ x 1) (- x 2) (/ (- x 4) 2))))

(define a 1)

(define mu
  (lambda (s n)
    (let ((mi (already-in? `(_ ,n _) s)))
      (if mi
        (mu s mi)
        n))))

(define m
  (lambda (s)
    (let ((n0 1))
      (mu s n0))))

(define F
  (lambda (_)
    (list (lambda (s)
            (list (lambda (n m)
                    (> (phi n) (phi m)))
                  (m s)
                  (+ (m s) a))))))

(define sat-phi?
  (lambda (x)
    (<= (phi x) (phi (+ x a)))))

(define final-state (find-state F '()))

; --> (sat-phi? (m final-state))
