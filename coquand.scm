(load "find-state-with-lists.scm")

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
	  ((eqv? n (list-ref (car sn) 1)) (list-ref (car sn) 2))
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
