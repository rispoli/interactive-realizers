(define sn+1
  (lambda (Fsn sn)
	(if (null? Fsn)
	  sn
	  (let ((fi-sn ((car Fsn) sn)))
		(if (consistent? fi-sn sn)
		  (incr-know fi-sn sn)
		  (sn+1 (cdr Fsn) sn))))))

(define find-state
  (lambda (F initial-state)
	(let ((Fsn (F initial-state)))
	  (let ((new-state (sn+1 Fsn initial-state)))
		(if (equal? new-state initial-state)
		  new-state
		  (find-state F new-state))))))
