(load "find-state.scm")

;;; if s = (<nu,n_1,m_1>, ..., <nu,n_k,m_k>):

(define already-in?
  (lambda (fi-sn sn)
	(cond
	  ((null? sn) #f)
	  ((member fi-sn sn) (list-ref fi-sn 2)) ; _ != #f ==> #t 
	  (else (already-in? fi-sn (cdr sn))))))

(define consistent?
  (lambda (fi-sn sn)
	(let ((n (list-ref fi-sn 1)) (m (list-ref fi-sn 2)))
	  (and (not (already-in? fi-sn sn)) ((car fi-sn) n m)))))

(define incr-know
  (lambda (fi-sn sn)
	(append sn (list fi-sn))))
