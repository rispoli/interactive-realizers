;#lang scheme

;; let f x y = x + y in
;; 	let z = 3 in
;; 		if [(true)|true] then
;; 			f (2 + 3 * 5) (3 + 1 - 1 * 2)
;; 		else
;; 			z
;; 
;; 
;; /usr/lib/plt/collects/parser-tools/examples/

(require parser-tools/yacc
		 parser-tools/lex
		 (prefix-in : parser-tools/lex-sre)
		 srfi/1)

(define-tokens value-tokens (NUM BOOL VAR))
(define-empty-tokens op-tokens (= OP CP + - * / ^ EOF NEG && \|\| ~ == != LET IN IF THEN ELSE))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (digit (:/ "0" "9")))

(define dsll
  (lexer
	((eof) 'EOF)
	((:or #\newline #\return #\tab #\space #\vtab) (dsll input-port))
	((:or "=" "+" "-" "*" "/" "^" "&&" "||" "~" "==" "!=") (string->symbol lexeme))
	("(" 'OP)
	(")" 'CP)
	("true" (token-BOOL #t))
	("false" (token-BOOL #f))
	("let" 'LET)
	("in" 'IN)
	("if" 'IF)
	("then" 'THEN)
	("else" 'ELSE)
	((:+ digit) (token-NUM (string->number lexeme)))
	((:+ (:or lower-letter upper-letter "_")) (token-VAR (string->symbol lexeme)))))

(define dslp
  (parser
	(start exp)
	(end EOF)
	(tokens value-tokens op-tokens)
	(error (lambda (tok-ok? tok-name tok-value)
			 (display "Error:")	; format ...
			 (display tok-name)
			 (display tok-value)))
	;(suppress)

	(precs (right = == !=)
		   (left - +)
		   (left * /)
		   (left NEG)
		   (left ^)
		   (left \|\|)
		   (left &&)
		   (left ~))

	(grammar
	  (ar-exp ((NUM) $1)
			  ((VAR) $1)
			  ((ar-exp + ar-exp) `(+ ,$1 ,$3))
			  ((ar-exp - ar-exp) `(- ,$1 ,$3))
			  ((ar-exp * ar-exp) `(* ,$1 ,$3))
			  ((ar-exp / ar-exp) `(/ ,$1 ,$3))
			  ((- ar-exp) (prec NEG) `(- ,$2))
			  ((ar-exp ^ ar-exp) `(expt ,$1 ,$3))
			  ((OP ar-exp CP) $2))
	  (bl-exp ((BOOL) $1)
			  ((VAR) $1)
			  ((bl-exp && bl-exp) `(and ,$1 ,$3))
			  ((bl-exp \|\| bl-exp) `(or ,$1 ,$3))
			  ((~ bl-exp) (prec ~) `(not ,$2))
			  ((ar-exp == ar-exp) `(= ,$1 ,$3))
			  ((ar-exp != ar-exp) `(not (= ,$1 ,$3)))
			  ((OP bl-exp CP) $2))
	  (exp-list (() '())
				((exp-list exp) (cons $2 $1)))
	  (var-list (() '())
				((var-list VAR) (cons $2 $1)))
	  (exp ((ar-exp) $1)
		   ((bl-exp) $1)
		   ((OP VAR exp-list CP) `(,$2 ,@(reverse $3)))
		   ((IF bl-exp THEN exp ELSE exp) `(if ,$2 ,$4 ,$6))
		   ((LET VAR var-list = exp IN exp) `(let ((,$2 ,(foldl (lambda (e p) `(lambda (,e) ,p)) $5 $3))) ,$7)))
	  )))
