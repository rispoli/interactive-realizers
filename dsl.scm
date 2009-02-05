;#lang scheme

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr)

(define-tokens value-tokens (NUM BOOL ID))
(define-empty-tokens op-tokens (dbl-semicolon semicolon colon = OP CP OCB CCB + - * / ^ EOF NEG && \|\| ~ == != > < >= <= @ LAMBDA -> LET LETREC IN IF THEN ELSE CASE DEFAULT WITH))

(define-lex-abbrevs
  (comment (:or (:: "//" (:* (:~ #\newline)) #\newline) (:: "/*" (complement (:: any-string "*/" any-string)) "*/")))
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  (digit (:/ "0" "9")))

(define dsll
  (lexer-src-pos
    ((eof) 'EOF)
    ((:or comment #\newline #\return #\tab #\space #\vtab) (return-without-pos (dsll input-port)))
    ((:or "=" "+" "-" "*" "/" "^" "&&" "||" "~" "==" "!=" ">" "<" ">=" "<=" "@" "->") (string->symbol lexeme))
    (";;" 'dbl-semicolon) (";" 'semicolon) (":" 'colon)
    ("(" 'OP) (")" 'CP) ("{" 'OCB) ("}" 'CCB) ; ("[" 'OSB) ("]" 'CSB)
    ("true" (token-BOOL #t))
    ("false" (token-BOOL #f))
    ("lambda" 'LAMBDA)
    ("let" 'LET)
    ("letrec" 'LETREC)
    ("in" 'IN)
    ("if" 'IF)
    ("then" 'THEN)
    ("else" 'ELSE)
    ("case" 'CASE)
    ("default" 'DEFAULT)
    ("with" 'WITH)
    ((:+ digit) (token-NUM (string->number lexeme)))
    ((:: (:+ (:or lower-letter upper-letter)) (:* (:or lower-letter upper-letter "_" digit))) (token-ID (string->symbol lexeme)))))

(define dslp
  (lambda (source-name)
    (parser
      (src-pos)
      (start start)
      (end dbl-semicolon EOF)
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
        (case-list (() '())
                   ((case-list DEFAULT colon exp semicolon) (cons `(else ,$4) $1))
                   ((case-list exp colon exp semicolon) (cons `((,$2) ,$4) $1)))
        (exp ((BOOL) $1)
             ((ID) $1)
             ((NUM) $1)
             ((OP exp CP) $2)
             ((prefix-op exp) (prec NEG) (list $1 $2))
             ((exp infix-op exp) (list $2 $1 $3))
             ((IF exp THEN exp ELSE exp) `(if ,$2 ,$4 ,$6))
             ((LET ID id-list = exp) (let ((body (WFR $2 #f $5 (list source-name $5-start-pos $5-end-pos))))
                                       `(define ,$2 ,(if (null? $3) body `(lambda (,@(reverse $3)) ,body)))))
             ((LET ID id-list = exp IN exp) (let ((body (WFR $2 #f $5 (list source-name $5-start-pos $5-end-pos))))
                                              `(let ((,$2 ,(if (null? $3) body `(lambda (,@(reverse $3)) ,body)))) ,$7)))
             ((LAMBDA id-list -> exp) `(lambda (,@(reverse $2)) ,$4))
             ((@ OP exp exp-list CP) `(,$3 ,@(reverse $4)))
             ((LETREC ID id-list = exp colon exp semicolon WITH exp) `(define ,$2 (lambda (,@(reverse $3)) (if ,(WFR $2 $10 $5) ,(WFR $2 $10 $7)))))
             ((LETREC ID id-list = CASE case-list WITH exp) `(define ,$2 (lambda (,@(reverse $3)) (cond ,@(WFR $2 $8 (reverse $6))))))
             ((OCB exp-list CCB) `(list ,@(reverse $2)))
             )))))

(define WFR
  (lambda (f weight code . forbidden)
    (letrec ((wfr
               (lambda (code)
                 (cond
                   ((null? code) '())
                   ((not (list? code)) code)
                   ((list? (car code)) (cons (wfr (car code)) (wfr (cdr code))))
                   (else
                     (case (car code)
                       ((if) `(if ,@(map (lambda (c) (wfr c)) (cdr code))))
                       ((let) (let ((vars '()))
                                `(let
                                   ,(map (lambda (c) (set! vars (cons (car c) vars)) (cons (car c) (wfr (cdr c)))) (cadr code))
                                   ,@(if (member f vars) (cddr code) (map (lambda (c) (wfr c)) (cddr code))))))
                       ((lambda) (if (member f (cadr code)) code `(lambda ,(cadr code) ,@(map (lambda (c) (wfr c)) (cddr code)))))
                       ((cond) `(cond ,@(map (lambda (c) `(,(wfr (car c)) ,(wfr (cadr c)))) (cdr code))))
                       ((f) (if (null? forbidden)
                              `(if (< (,weight ,(cadr code)) (,weight ,(cadadr code))) ,code 0)
                              (let* ((forbidden (car forbidden)) (source (list-ref forbidden 0)) (start (list-ref forbidden 1)) (end (list-ref forbidden 2)))
                                (raise-read-error (format "forbidden recursion on ~a" f) source (position-line start) (position-col start) (position-offset start) (- (position-offset end) (position-offset start))))))
                       (else (cons (car code) (wfr (cdr code))))))))))
      (wfr code))))

(define !=
  (lambda (x y)
    (not (= x y))))

(define translate
  (lambda (s . src-name)
    (let ((ois (open-input-string s)) (statements '()))
      (port-count-lines! ois)
      (letrec ((loop
                 (lambda ()
                   (let ((r ((dslp (if (empty? src-name) "current-input-port" (car src-name))) (lambda () (dsll ois)))))
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
                            (translate (port->string in) path)))))

(define evaluate-file
  (lambda (path)
    (map eval (translate-file path)))) ; (#<void> ... ) for-each to ignore the results? http://docs.plt-scheme.org/reference/pairs.html#(def._((lib._scheme/private/map..ss)._for-each))

; (translate "5 + if true then 2 else 3")
; (translate "let f x y = x + y in let z = 3 in if false then @(f (2+3*5) (3+1-1*2)) else z")
; (translate "let phi x = (x+1)*(x-2)*((x-4)/2)")
; (translate "let x = 3")
; (translate "lambda n m -> @(phi n) + @(phi m)")
; (translate "@((lambda x y -> x + y) 2 3)")
; (translate "letrec f x = case P1: @(f @(g x)); P2: t2; P3: if true then @(f @(i x)) else @(f @(i y)); P4: @(f @(j x)); with w")
; (translate "letrec f x = case P1: @(f @(g x)); P2: t2; P3: if true then @(f @(i x)) else @(f @(i y)); P4: @(f @(j x)); default: @(f @(t x)); with w")
; (translate "let f x y = @(f x + y) in @(f 2 3)") ; forbidden
; (translate "let f x y = @(f x + y)") ; forbidden
; (translate "letrec f x = P1: let h x y = @(g x + y) in @(f @(g x)); with W") ; transformed
; (translate "letrec f x = P1: let f x y = @(g x + y) in @(f @(g x)); with W") ; shadowed
; (translate "letrec f x = P1: let f x y = @(f x + y) in @(f @(g x)); with W") ; forbidden
; (translate "letrec f x = @(f @(h y)): @(f @((lambda x -> 3) x)); with W")
; (translate "letrec f x = case P1: @(f @(g x)); @(f @(h y)): t2; default: @(f @(i z)); with W")
; (translate "letrec f x = case default: @(f @(i z)); with W")
