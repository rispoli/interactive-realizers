(load "find-state.scm")

;;; if s = (<nu,n_1,m_1>, ..., <nu,n_k,m_k>):

(define eqv-p?
  (lambda (pattern target)
    (let ((i -1))
      (foldl (lambda (a b) (and a b))
             #t
             (map (lambda (p)
                    (set! i (+ i 1))
                    (if (equal? p '_)
                      #t
                      (eqv? p (list-ref target i))))
                  pattern)))))

(define already-in?
  (lambda (fi-sn sn)
    (cond
      ((null? sn) #f)
      ((eqv-p? fi-sn (car sn)) (list-ref (car sn) 2))
      (else (already-in? fi-sn (cdr sn))))))

(define find-m
  (lambda (s n)
    (already-in? `(_ ,n _) s)))

(define consistent?
  (lambda (fi-sn sn)
    (let ((P (list-ref fi-sn 0)) (n (list-ref fi-sn 1)) (m (list-ref fi-sn 2)))
      (and (not (already-in? `(,P ,n _) sn)) (P n m)))))

(define incr-know
  (lambda (fi-sn sn)
    (append sn (list fi-sn))))

(define state-consistent?
  (lambda (s)
    (foldl (lambda (a b) (and a b))
           #t
           (map (lambda (e)
                  (let ((P (list-ref e 0)) (x (list-ref e 1)) (y (list-ref e 2)))
                    (P x y)))
                s))))
