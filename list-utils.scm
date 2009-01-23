(define randomth
  (lambda (l)
    (let ((n (random (length l))))
      (list-ref l n))))

(define factorial
  (lambda (n)
    (let loop ((n n) (product 1))
      (if (= n 0)
        product
        (loop (- n 1) (* product n))))))

;;; http://en.literateprograms.org/Kth_permutation_(OCaml)

(define rr
  (lambda (n k)
    (if (= n 0)
      '()
      (cons (modulo k n) (rr (- n 1) (quotient k n))))))

(define dfr
  (lambda (xs)
    (foldr (lambda (x rs)
             (cons x (map (lambda (r)
                            (+ r (if (<= x r)
                                   1
                                   0)))
                          rs)))
           '()
           xs)))

(define perm
  (lambda (xs k)
    (map (lambda (n)
           (list-ref xs n))
         (dfr (rr (length xs) k)))))

(define all-perm
  (lambda (l)
    (let ((n! (factorial (length l))))
      (letrec ((ap (lambda (i acc)
                     (if (>= i n!)
                       acc
                       (ap (+ i 1) (cons (perm l i) acc))))))
        (ap 0 '())))))
