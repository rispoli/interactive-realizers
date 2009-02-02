(load "find-state-with-lists.scm")
(load "list-utils.scm")

(define phi
  (lambda (x)
    ;(* (+ x 1) (- x 2) (/ (- x 4) 2))))
    (sin x)))

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
  (lambda (n)
    (letrec ((F (lambda (n acc)
                  (if (= n 0)
                    acc
                    (let ((op (randomth (list + - * /))) (coeff (+ (random 7) 1)))
                      (printf "op: ~a, coeff: ~a~%" op coeff)
                      (F (- n 1) (cons (lambda (s)
                                         (list (lambda (n m)
                                                 (> (phi n) (phi m)))
                                               (m s)
                                               (op (m s) coeff)))
                                       acc)))))))
      (F n '()))))

(define fs '())

(define initial-state '())

(define final-state '())

(define find-solutions
  (lambda (n . nfs)
    (set! fs (if (null? nfs)
               (F n)
               (car nfs)))
    (set! final-state (find-state (lambda (_) fs) initial-state))
    (m final-state)))

(define to-gnuplot
  (lambda (filename)
    (with-output-to-file filename
                         (lambda ()
                           (printf "f(x) = ~%")
                           (printf "plot f(x)~%")
                           (printf "replot f(~a)~%" (m final-state))
                           (let ((fs-final-state (map (lambda (p) (list-ref (p final-state) 2)) fs)))
                             (map (lambda (v) (printf "replot f(~a)~%" v)) fs-final-state)
                             (printf "set xrange[~a:~a]; replot~%" (- (apply min fs-final-state) 1) (+ (apply max fs-final-state) 1))
                             (printf "set yrange[:]; replot~%"))))))

(define all-perm-to-gnuplot
  (lambda (filename)
    (let ((perms (all-perm fs)) (i 0) (n (length fs)))
      (map (lambda (f)
             (set! i (+ i 1))
             (let ((current-solution (find-solutions n f)))
               (let ((filename (format "~a.~a" filename i)))
                 (printf "Saving results for permutation ~a in ~a~%" i filename)
                 (to-gnuplot filename))
               current-solution))
           perms))))
