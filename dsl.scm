; Lexer and Grammar inspired from Ocaml Light:
;   compiler/parser.mly
;   compiler/lexer.mlp

;#lang scheme

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr)

(define-tokens value-tokens (NUM BOOL ID INFIX0 INFIX2 INFIX3 INFIX4))
(define-empty-tokens op-tokens (EOF EQUAL EQUALEQUAL AMPERSAND LPAREN RPAREN MINUSGREATER COLON SEMI SEMISEMI LBRACKET RBRACKET AMPERAMPER BARBAR CASE DEFAULT ELSE FN IF IN LAMBDA LET LETREC NOT OR SUBTRACTIVE THEN WITH prec_let prec_uminus prec_app prec_list prec_if))

(define-lex-abbrevs
  ;(comment (:or (:: "//" (:* (:~ #\newline)) #\newline) (:: "/*" (complement (:: any-string "*/" any-string)) "*/"))) ; C style
  (comment (:: "(*" (complement (:: any-string "*)" any-string)) "*)")) ; OCaml style
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (digit (:/ "0" "9")))

(define dsll
  (lexer-src-pos
    ((eof) 'EOF)
    ((:or comment #\newline #\return #\tab #\space #\vtab) (return-without-pos (dsll input-port)))
    ("-" 'SUBTRACTIVE)
    ((:or "!=" "<" ">" "<=" ">=") (token-INFIX0 (string->symbol lexeme)))
    ((:or "+" "-") (token-INFIX2 (string->symbol lexeme)))
    ((:or "*" "/") (token-INFIX3 (string->symbol lexeme)))
    ((:or "^" "**") (token-INFIX4 (string->symbol "expt")))
    ("=" 'EQUAL)
    ("==" 'EQUALEQUAL)
    ("&" 'AMPERSAND)
    ("(" 'LPAREN)
    (")" 'RPAREN)
    ("->" 'MINUSGREATER)
    (":" 'COLON)
    (";" 'SEMI)
    (";;" 'SEMISEMI)
    ("[" 'LBRACKET)
    ("]" 'RBRACKET)
    ("&&" 'AMPERAMPER)
    ("||" 'BARBAR)
    ("case" 'CASE)
    ("default" 'DEFAULT)
    ("else" 'ELSE)
    ("false" (token-BOOL #f))
    ("fn" 'FN)
    ("if" 'IF)
    ("in" 'IN)
    ("lambda" 'LAMBDA)
    ("let" 'LET)
    ("letrec" 'LETREC)
    ("not" 'NOT)
    ("or" 'OR)
    ("then" 'THEN)
    ("true" (token-BOOL #t))
    ("with" 'WITH)
    ((:+ digit) (token-NUM (string->number lexeme)))
    ((:: (:+ (:or lower-letter upper-letter)) (:* (:or lower-letter upper-letter "_" digit))) (token-ID (string->symbol lexeme)))))

(define dslp
  (lambda (source-name)
    (parser
      (src-pos)
      (start start)
      (end SEMISEMI EOF)
      (tokens value-tokens op-tokens)
      (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
               (raise-read-error
                 (format "unexpected ~a" tok-name)
                 source-name
                 (position-line start-pos)
                 (position-col start-pos)
                 (position-offset start-pos)
                 (- (position-offset end-pos)
                    (position-offset start-pos)))))

      (precs (right prec_let)
             (right MINUSGREATER)
             (right SEMI)
             (right prec_list)
             (right prec_if)
             (left OR BARBAR)
             (left AMPERSAND AMPERAMPER)
             (left NOT)
             (left INFIX0 EQUAL EQUALEQUAL)
             (left INFIX2 SUBTRACTIVE)
             (left INFIX3)
             (right INFIX4)
             (right prec_uminus)
             (right prec_app))

      (grammar
        (start (() #f)
               ((toplevel) $1))
        (case-list ((expr COLON expr SEMI case-list-default) (cons `((,$1) ,$3) $5))
                   ((expr COLON expr SEMI) (list `((,$1) ,$3))))
        (case-list-default ((case-list) $1)
                           ((DEFAULT COLON expr SEMI) (list `(else ,$3))))
        (expr ((simple-expr) $1)
              ((simple-expr simple-expr-list) (prec prec_app) `(,$1 ,@$2))
              ((SUBTRACTIVE expr) (prec prec_uminus) `(- ,$2))
              ((NOT expr) `(not ,$2))
              ((expr INFIX4 expr) (list $2 $1 $3))
              ((expr INFIX3 expr) (list $2 $1 $3))
              ((expr INFIX2 expr) (list $2 $1 $3))
              ((expr SUBTRACTIVE expr) `(- ,$1 ,$3))
              ((expr INFIX0 expr) (list $2 $1 $3))
              ((expr EQUALEQUAL expr) `(= ,$1 ,$3))
              ((expr AMPERSAND expr) `(and ,$1 ,$3))
              ((expr AMPERAMPER expr) `(and ,$1 ,$3))
              ((expr OR expr) `(or ,$1 ,$3))
              ((expr BARBAR expr) `(or ,$1 ,$3))
              ((IF expr THEN expr ELSE expr) (prec prec_if) `(if ,$2 ,$4 ,$6))
              ((LET id-list EQUAL expr IN expr) (prec prec_let) (let ((body (WFR (car $2) #f $4 #:forbidden (list source-name $4-start-pos $4-end-pos))))
                                                                  `(let ((,(car $2) ,(if (null? (cdr $2)) body `(lambda (,@(cdr $2)) ,body)))) ,$6)))
              ((function id-list MINUSGREATER expr) `(lambda (,@$2) ,$4)))
        (expr-sm-list ((expr-sm-list SEMI expr) (prec prec_list) (cons $3 $1))
                      ((expr) (prec prec_list) (list $1))
                      ((expr-sm-list SEMI) $1)
                      (() '()))
        (function ((LAMBDA) 'lambda)
                  ((FN) 'lambda))
        (id-list ((ID id-list) (cons $1 $2))
                 ((ID) (list $1)))
        (toplevel ((expr) $1)
                  ((LET id-list EQUAL expr) (prec prec_let) (let ((body (WFR (car $2) #f $4 #:forbidden (list source-name $4-start-pos $4-end-pos))))
                                                              `(define ,(car $2) ,(if (null? (cdr $2)) body `(lambda (,@(cdr $2)) ,body)))))
                  ((LETREC id-list EQUAL CASE case-list-default WITH expr) `(define ,(car $2) (lambda (,@(cdr $2)) (cond ,@(WFR (car $2) $7 $5)))))
                  ((LETREC id-list EQUAL expr COLON expr SEMI WITH expr) `(define ,(car $2) (lambda (,@(cdr $2)) (if ,(WFR (car $2) $9 $4) ,(WFR (car $2) $9 $6) #f)))))
        (simple-expr ((BOOL) $1)
                     ((ID) $1)
                     ((NUM) $1)
                     ((LPAREN expr RPAREN) $2)
                     ((LBRACKET expr-sm-list RBRACKET) `(list ,@(reverse $2))))
        (simple-expr-list ((simple-expr simple-expr-list) (cons $1 $2))
                          ((simple-expr) (list $1)))))))

(define WFR
  (lambda (f weight code #:forbidden (forbidden '()))
    (letrec ((wfr
               (lambda (code)
                 (cond
                   ((null? code) '())
                   ((not (list? code)) code)
                   ((list? (car code)) (cons (wfr (car code)) (wfr (cdr code))))
                   ((eqv? f (car code)) (if (null? forbidden)
                                          `(if (< (,weight ,(cadr code)) (,weight ,(cadadr code))) ,code 0)
                                          (let* ((source (list-ref forbidden 0)) (start (list-ref forbidden 1)) (end (list-ref forbidden 2)))
                                            (raise-read-error (format "forbidden recursion on ~a" f) source (position-line start) (position-col start) (position-offset start) (- (position-offset end) (position-offset start))))))
                   (else
                     (case (car code)
                       ((if) `(if ,@(map (lambda (c) (wfr c)) (cdr code))))
                       ((let) (let ((vars '()))
                                `(let
                                   ,(map (lambda (c) (set! vars (cons (car c) vars)) (cons (car c) (wfr (cdr c)))) (cadr code))
                                   ,@(if (member f vars) (cddr code) (map (lambda (c) (wfr c)) (cddr code))))))
                       ((lambda) (if (member f (cadr code)) code `(lambda ,(cadr code) ,@(map (lambda (c) (wfr c)) (cddr code)))))
                       ((cond) `(cond ,@(map (lambda (c) `(,(wfr (car c)) ,(wfr (cadr c)))) (cdr code))))
                       (else (cons (car code) (wfr (cdr code))))))))))
      (wfr code))))

(define !=
  (lambda (x y)
    (not (= x y))))

(define translate
  (lambda (s #:src-name (src-name "current-input-port"))
    (let ((ois (open-input-string s)) (statements '()))
      (port-count-lines! ois)
      (letrec ((loop
                 (lambda ()
                   (let ((r ((dslp src-name) (lambda () (dsll ois)))))
                     (when r
                       (set! statements (cons r statements))
                       (loop))))))
        (loop))
      (reverse statements))))

(define evaluate
  (lambda (s)
    (for-each eval (translate s))))

(define translate-file
  (lambda (path)
    (call-with-input-file path
                          (lambda (in)
                            (translate (port->string in) #:src-name path)))))

(define evaluate-file
  (lambda (path)
    (for-each eval (translate-file path))))

; (translate "5 + if true then 2 else 3")
; (translate "let f x y = x + y in let z = 3 in if false then f (2+3*5) (3+1-1*2) else z")
; (translate "let phi x = (x+1)*(x-2)*((x-4)/2)")
; (translate "let x = 3")
; (translate "lambda n m -> phi n + phi m")
; (translate "(lambda x y -> x + y) 2 3")
; (translate "letrec f x = case P1: f (g x); P2: t2; P3: if true then f (i x) else f (i y); P4: f (j x); with w")
; (translate "letrec f x = case P1: f (g x); P2: t2; P3: if true then f (i x) else f (i y); P4: f (j x); default: f (t x); with w")
; (translate "let f x y = f (x + y) in f 2 3") ; forbidden
; (translate "let f x y = f (x + y)") ; forbidden
; (translate "letrec f x = P1: let h x y = g (x + y) in f (g x); with W") ; transformed
; (translate "letrec f x = P1: let f x y = g (x + y) in f (g x); with W") ; shadowed
; (translate "letrec f x = P1: let f x y = f (x + y) in f (g x); with W") ; forbidden
; (translate "letrec f x = f (h y): f ((lambda x -> 3) x); with W") ; (if 0 ...) ~ (if #f ...) ???
; (translate "letrec f x = case P1: f (g x); f (h y): t2; default: f (i z); with W")
; (translate "letrec f x = case default: f (i z); with W")
