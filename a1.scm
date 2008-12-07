;;; Algoritmo generale (indipendente dalla rappresentazione degli stati o
;;; dall'esempio):

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

;;; Se s = (<nu,n_1,m_1>, ..., <nu,n_k,m_k>):

(define already-in?
  (lambda (fi-sn sn)
	(cond
	  ((null? sn) #f)
	  ((member fi-sn sn) (caddr fi-sn)) ; _ != #f ==> #t 
	  (else (already-in? fi-sn (cdr sn))))))

(define consistent?
  (lambda (fi-sn sn)
	(let ((n (cadr fi-sn)) (m (caddr fi-sn)))
	  (and (not (already-in? fi-sn sn)) ((car fi-sn) n m)))))

(define incr-know
  (lambda (fi-sn sn)
	(append sn (list fi-sn))))

;;; Esempio Coquand:

(define phi
  (lambda (x)
	(* (+ x 1) (- x 2) (/ (- x 4) 2))))

(define a 1)

;; Codice praticamente uguale a already-in?:
;; sarebbe meglio poter sostituire questo codice e relativa chiamata (nella
;; definizione di mu) con qualcosa del tipo: (already-in? (_, n, _) s) dove gli
;; _ abbiano lo stesso significato che hanno in Prolog. Qualche suggerimento a
;; riguardo?
(define find-m
  (lambda (n sn)
	(cond
	  ((null? sn) #f)
	  ((eqv? n (cadr (car sn))) (caddr (car sn)))
	  (else (find-m n (cdr sn))))))

(define mu
  (lambda (s n)
	(let ((mi (find-m n s)))
	  (if mi
		(mu s mi)
		n))))

(define m
  (lambda (s)
	(let ((n0 1))
	  (mu s n0))))

;; Utilizzando al posto di un nome generico il predicato (o un qualche
;; "puntatore" ad esso, come nell'articolo) l'implementazione di 
;; consistent? risulta piu' facilmente generalizzabile: se ci fosse solo un nome
;; e non un modo per risalire da quest'ultimo al predicato bisognerebbe
;; passargliero in qualche altra maniera.
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
