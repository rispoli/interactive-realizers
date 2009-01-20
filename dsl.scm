;#lang scheme

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         srfi/1)

(define-tokens value-tokens (NUM BOOL ID))
(define-empty-tokens op-tokens (dbl-semicolon = OP CP + - * / ^ EOF NEG && \|\| ~ == != > < >= <= @ LAMBDA -> LET LETREC IN IF THEN ELSE))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (digit (:/ "0" "9")))

(define dsll
  (lexer
    ((eof) 'EOF)
    ((:or #\newline #\return #\tab #\space #\vtab) (dsll input-port))
    ((:or "=" "+" "-" "*" "/" "^" "&&" "||" "~" "==" "!=" ">" "<" ">=" "<=" "@" "->") (string->symbol lexeme))
    (";;" 'dbl-semicolon)
    ("(" 'OP)
    (")" 'CP)
    ("true" (token-BOOL #t))
    ("false" (token-BOOL #f))
    ("lambda" 'LAMBDA)
    ("let" 'LET)
    ("letrec" 'LETREC)
    ("in" 'IN)
    ("if" 'IF)
    ("then" 'THEN)
    ("else" 'ELSE)
    ((:+ digit) (token-NUM (string->number lexeme)))
    ((:: (:+ (:or lower-letter upper-letter)) (:* (:or lower-letter upper-letter "_" digit))) (token-ID (string->symbol lexeme)))))

(define dslp
  (parser
    (start start)
    (end dbl-semicolon EOF)
    (tokens value-tokens op-tokens)
    (error (lambda (tok-ok? tok-name tok-value)
             (printf "Error: ~a ~a ~a~%" tok-ok? tok-name tok-value)))
    ;(suppress)

    (precs (right = == != > < >= <=)
           (left - +)
           (left * /)
           (left NEG)
           (left ^)
           (left \|\|)
           (left &&)
           (left ~))

    (grammar
      (start (() #f)
             ((exp) $1))
      (prefix-op ((-) '-) ((~) 'not))
      (infix-op ((+) '+) ((-) '-) ((*) '*) ((/) '/) ((^) 'expt) ((&&) 'and) ((\|\|) 'or) ((==) '=) ((!=) '!=) ((>) '>) ((<) '<) ((>=) '>=) ((<=) '<=))
      (id-list (() '())
               ((id-list ID) (cons $2 $1)))
      (exp-list (() '())
                ((exp-list exp) (cons $2 $1)))
      (exp ((BOOL) $1)
           ((ID) $1)
           ((NUM) $1)
           ((OP exp CP) $2)
           ((prefix-op exp) (prec NEG) (list $1 $2))
           ((exp infix-op exp) (list $2 $1 $3))
           ((IF exp THEN exp ELSE exp) `(if ,$2 ,$4 ,$6))
           ((LET ID id-list = exp) `(define ,$2 ,(if (> (length $3) 0) `(lambda (,@(reverse $3)) ,$5) $5)))
           ((LET ID id-list = exp IN exp) `(let ((,$2 ,(if (> (length $3) 0) `(lambda (,@(reverse $3)) ,$5) $5))) ,$7)) ; uncurried
           ((LAMBDA id-list -> exp) `(lambda (,@(reverse $2)) ,$4))
           ((@ OP exp exp-list CP) `(,$3 ,@(reverse $4)))))))

(define !=
  (lambda (x y)
    (not (= x y))))

;; contare le righe per emettere eventuale errore con informazione? http://docs.plt-scheme.org/guide/exns.html
(define translate
  (lambda (s)
    (let ((ois (open-input-string s)) (statements '()))
      ;(port-count-lines! ois)
      (letrec ((loop
                 (lambda ()
                   (let ((r (dslp (lambda () (dsll ois)))))
                     (when r
                       (set! statements (cons r statements))
                       (loop))))))
        (loop))
      (reverse statements))))

(define evaluate
  (lambda (s)
    (map eval (translate s)))) ; (#<void> ... ) for-each to ignore the results? http://docs.plt-scheme.org/reference/pairs.html#(def._((lib._scheme/private/map..ss)._for-each))

(define translate-file
  (lambda (path)
    (call-with-input-file path
                          (lambda (in)
                            (translate (port->string in))))))

(define evaluate-file
  (lambda (path)
    (map eval (translate-file path)))) ; (#<void> ... ) for-each to ignore the results? http://docs.plt-scheme.org/reference/pairs.html#(def._((lib._scheme/private/map..ss)._for-each))

; (translate "5 + if true then 2 else 3")
; (translate "let f x y = x + y in let z = 3 in if false then @(f (2+3*5) (3+1-1*2)) else z")
; (translate "let phi x = (x+1)*(x-2)*((x-4)/2)")
; (translate "let x = 3")
; (translate "lambda n m -> @(phi n) + @(phi m)")
; (translate "@((lambda x y -> x + y) 2 3)")
