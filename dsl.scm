;#lang scheme

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         srfi/1)

(define-tokens value-tokens (NUM BOOL VAR))
(define-empty-tokens op-tokens (= OP CP + - * / ^ EOF NEG && \|\| ~ == != > < >= <= @ LAMBDA -> LET LETREC IN IF THEN ELSE))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (digit (:/ "0" "9")))

(define dsll
  (lexer
    ((eof) 'EOF)
    ((:or #\newline #\return #\tab #\space #\vtab) (dsll input-port))
    ((:or "=" "+" "-" "*" "/" "^" "&&" "||" "~" "==" "!=" ">" "<" ">=" "<=" "@" "->") (string->symbol lexeme))
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
    ((:: (:+ (:or lower-letter upper-letter)) (:* (:or lower-letter upper-letter "_" digit))) (token-VAR (string->symbol lexeme)))))

(define dslp
  (parser
    (start exp)
    (end EOF)
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
      (prefix-op ((-) '-) ((~) 'not))
      (infix-op ((+) '+) ((-) '-) ((*) '*) ((/) '/) ((^) 'expt) ((&&) 'and) ((\|\|) 'or) ((==) '=) ((!=) '!=) ((>) '>) ((<) '<) ((>=) '>=) ((<=) '<=))
      (var-list (() '())
                ((var-list VAR) (cons $2 $1)))
      (exp-list (() '())
                ((exp-list exp) (cons $2 $1)))
      (exp ((BOOL) $1)
           ((NUM) $1)
           ((VAR) $1)
           ((OP exp CP) $2)
           ((prefix-op exp) (prec NEG) (list $1 $2))
           ((exp infix-op exp) (list $2 $1 $3))
           ((IF exp THEN exp ELSE exp) `(if ,$2 ,$4 ,$6))
           ((LET VAR var-list = exp) `(define ,$2 ,(if (> (length $3) 0) `(lambda (,@(reverse $3)) ,$5) $5)))
           ((LET VAR var-list = exp IN exp) `(let ((,$2 ,(if (> (length $3) 0) `(lambda (,@(reverse $3)) ,$5) $5))) ,$7)) ; uncurried
           ((LETREC VAR var-list = exp IN exp) `(letrec ((,$2 ,(if (> (length $3) 0) `(lambda (,@(reverse $3)) ,$5) $5))) ,$7)) ; uncurried
           ((LAMBDA var-list -> exp) `(lambda (,@(reverse $2)) ,$4))
           ((@ OP exp exp-list CP) `(,$3 ,@(reverse $4)))))))

(define !=
  (lambda (x y)
    (not (= x y))))

(define c
  (lambda (s)
    (let ((ois (open-input-string s)))
      (dslp (lambda ()
              (dsll ois))))))

(define e
  (lambda (s)
    (eval (c s))))

; (c "5 + if true then 2 else 3")
; (c "let f x y = x + y in let z = 3 in if false then @(f (2+3*5) (3+1-1*2)) else z")
; (c "let phi x = (x+1)*(x-2)*((x-4)/2)")
; (c "let x = 3")
; (c "lambda n m -> @(phi n) + @(phi m)")
; (c "@((lambda x y -> x + y) 2 3)")
