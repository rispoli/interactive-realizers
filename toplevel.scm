;#lang scheme

(load "dsl.scm")
(load "find-state-with-lists.scm")

(require (planet vyzo/gnuplot)) ; docs @ http://planet.plt-scheme.org/package-source/vyzo/gnuplot.plt/1/3/planet-docs/manual/index.html

;(gnuplot-set-program! path-string)

(define final-state '())

(define find-solution
  (lambda (filename F #:initial-state (initial-state '()))
    (evaluate-file filename)
    (set! final-state (find-state (eval F) initial-state))
    (printf "valid solution in x = ~a~%" (m final-state))))

(define gplot (gnuplot-spawn))

(define data
  (lambda (functions #:points (points 200) #:step (step 10.))
    (gnuplot-data
      (build-list points
                  (lambda (x)
                    (let ((x (/ x step))) ; approximation problems with *
                      (cons x (map (lambda (f) (f x)) functions))))))))

(define plot
  (lambda (functions-name data #:range-x (range-x '()) #:range-y (range-y '()) #:title (title ""))
    (gnuplot-set gplot `(title (str ,title)))
    (apply gnuplot-plot (cons gplot
                              (let ((n 1))
                                (map (lambda (x)
                                       (set! n (+ 1 n))
                                       (gnuplot-item data `(using (seq: 1 ,n) title (str ,x) with line)))
                                     functions-name)))
           #:range `(,range-x ,range-y))))

; saving to file before plotting on screen yields an empty file
(define to-png
  (lambda (filename)
    (let ((png '(png enhanced)))
      (gnuplot-hardcopy gplot filename #:term png))))

(define to-gnuplot
  (lambda (functions #:delta (delta 3))
    (let ((functions-e (map (lambda (x) (eval x)) functions)))
      (let ((data (data functions-e)) (sol (m final-state)))
        (plot (map (lambda (x) (format "~a(x)" x)) functions) data #:range-x `(,(- sol delta) ,(+ delta sol)))))))

; (find-solution "minimo.dsl" 'F)
; (to-gnuplot '(g (compose f g) h (compose f h)))

; (find-solution "coquand.dsl" 'F)
; (to-gnuplot '(phi (lambda (x) (phi (+ 1 x)))))
