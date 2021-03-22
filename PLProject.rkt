
#lang racket
(require (lib "eopl.ss" "eopl"))

; import library for parsing and lexing
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

;lexer implementation to identify tokens 
(define simple-math-lexer
           (lexer
            [digits (token-NUM (string->number lexeme))]
            [(:or (:seq (:? digits) "." digits)
                (:seq digits "."))
                (token-NUM (string->number lexeme))]
            [(:or "+") (token-plus)]
            [(:or "-") (token-minus)]
            [(:or "*") (token-mul)]
            [(:or "/") (token-div)]
            [(:or ">") (token-grt)]
            [(:or "=") (token-assignment)]
            [(:or "<") (token-les)]
            [(:or "!=") (token-neq)]
            [(:or "==") (token-eqq)]
            [(:or "false") (token-false)]
            [(:or "true") (token-true)]
            [(:or "return") (token-return)]
            [(:or "null") (token-null)]
            [(:or "do") (token-do)]
            [(:or "while") (token-while)]
            [(:or "end") (token-end)]
            [(:or "if") (token-if)]
            [(:or "else") (token-else)]
            [(:or "then") (token-then)]
            [(:or "(") (token-right-par)]
            [(:or ")") (token-left-par)]
            [(:or "[") (token-left-brac)]
            [(:or "]") (token-right-brac)]
            [(:or ";") (token-semicol)]
            [(:or ",") (token-separ)]
            [(repetition 1 +inf.0 (union (char-range #\a #\z) (char-range #\A #\Z))) (token-variable lexeme)]
            [(:seq "\"" alphabet "\"") (token-string lexeme)]
            (whitespace (simple-math-lexer input-port))
            ((eof) (token-EOF))))

; define tokens used in the grammar
(define-tokens a (NUM))
(define-tokens b (string))
(define-tokens c (variable))
(define-empty-tokens g (EOF plus minus mul div grt assignment les neq eqq
    true false null return while do end if then else left-par right-par
    left-brac right-brac semicol separ)
)
; define abbreviation used in lexing process 
(define-lex-abbrevs
  (digits (:+ (char-set "0123456789")))
  (letter (:+ (union (char-range #\a #\z) (char-range #\A #\Z))))
  (alphabet (:+ alphabetic))
)


;parser implementation for building a parser-tree
(define simple-math-parser
           (parser
            (start command)
            (end EOF)
            (error void)
            (tokens a b c g)
            (grammar
              (command ((keyword) (a-program (single-command $1))) ((command semicol keyword) (a-program (multi-command $1 $3))))
              (keyword ((while_statement) $1) ((if_statement) $1)  ((assignment_statement) $1) ((Return) (return $1)))
              (while_statement ((while exp do command end) (while_statement $2 $4)))
              (if_statement ((if exp then command else command end) (if_statement $2 $4 $6)))
              (assignment_statement ((variable assignment exp) (assignment_statement $1 $3)))
              (Return ((return exp) $2)) 
              (exp ((aexp) (aexpression $1)) ((aexp grt aexp) (grt $1 $3)) ((aexp les aexp) (les $1 $3)) ((aexp eqq aexp) (eqq $1 $3))
                  ((aexp neq aexp) (neq $1 $3))
              )
              (aexp ((bexp) (bexpression $1)) ((bexp minus aexp) (minus $1 $3)) ((bexp plus aexp) (plus $1 $3)))
              (bexp ((cexp) (cexpression $1)) ((cexp mul bexp) (mul $1 $3)) ((cexp div bexp) (div $1 $3)))
              (cexp ((minus cexp) (neg $2)) ((left-par exp right-par) (par $2)) ((NUM) (number $1)) ((null) (null)) ((variable) (vari $1)) ((true) (boolean #t))
                    ((false) (boolean #f)) ((string) (string $1)) ((list) (mk-list $1)) ((variable listmember) (indexing $1 $2)))
              (list ((left-brac listValues right-brac) (a-list $2)) ((left-brac right-brac) (empty-list)));list of racket doesn't infere with this list in grammar? // check outputs
              (listValues ((exp) (oneelement $1)) ((exp separ listValues) (nelement $1 $3)))
              (listmember ((left-brac exp right-brac) (oned $2)) ((left-brac exp right-brac listmember) (nd $2 $4)))

            )))




; environment datatype
(define-datatype env-type env?

  (empty-env); 1st variaant
  (extend-env [var string?] ; 2nd variant
              [val always?]
              [env env?]))

(define apply-env
  (lambda (env search-var)
    (cases env-type env
      [empty-env () (raise (string-append "name " search-var " is not defined"))]
      [extend-env (var val env) (if (equal? var search-var)
                                    val
                                    (apply-env env search-var))])))

(define init-env
  (lambda ()
    (empty-env)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; program datatype
(define-datatype program program?
  (a-program
   (exp1 command?)))
;;;;;;;

; command datatype
(define-datatype command command?
  (multi-command
   (exp1 program?)
   (exp2 keyword?))
  (single-command
   (exp1 keyword?))
)
;;;;;;;;;;;

;keyword datatype 
(define-datatype keyword keyword?
  (if_statement
   (cond expression?)
   (body program?)
   (else-body program?))
  (while_statement
    (cond expression?)
    (body program?))
  (assignment_statement
   (var string?)
   (exp1 expression?))
  (return
    (exp1 expression?)))
;;;;;;;;;;;;;;;;;;;

; expression datatype
(define-datatype expression expression?
  (aexpression
    (aexp aexp?))
  (grt
    (aexp1 aexp?)
    (aexp2 aexp?))
  (les
    (aexp1 aexp?)
    (aexp2 aexp?))
  (eqq
    (aexp1 aexp?)
    (aexp2 aexp?))
  (neq
    (aexp1 aexp?)
    (aexp2 aexp?)))
;;;;;;;;;;;;;;;;;;;;

; aexp datatype
(define-datatype aexp aexp?
  (bexpression
    (bexp bexp?))
  (plus
    (bexp bexp?)
    (aexp aexp?))
  (minus
    (bexp bexp?)
    (aexp aexp?)))
;;;;;;;;;;;;;;;;

; bexp datatype
(define-datatype bexp bexp?
  (cexpression
    (cexp cexp?))
  (mul
    (cexp cexp?)
    (bexp bexp?))
  (div
    (cexp cexp?)
    (bexp bexp?))
)
;;;;;;;;;;;;

;cexp datatype
(define-datatype cexp cexp?
  (number
    (num number?))
  (string
    (str string?))
  (null)
  (boolean
    (bool boolean?))
  (neg
    (exp1 cexp?))
  (par
    (exp1 expression?))
  (mk-list
    (l mylist?))
  (indexing
    (var string?)
    (index listmember?))
  (vari
    (var string?)))
;;;;;;;;;;;;;;;;;;

;mylist datatype
(define-datatype mylist mylist?
  (a-list
   (listvals listValues?))
  (empty-list))
;;;;;;;;;;;;;;;;

; listValues datatype
(define-datatype listValues listValues?
  (oneelement
    (exp1 expression?))
  (nelement
    (exp1 expression?)
    (values listValues?)))
;;;;;;;;;;;;;;;;;

; listmember datatype
(define-datatype listmember listmember?
  (oned 
    (exp1 expression?))
  (nd 
    (exp1 expression?)
    (exp2 listmember?)))
;;;;;;;;;;;;;;;;;;;;

; evaluate a program
; program is a datatype wrapps command
(define value-of-program 
  (lambda (pgm env) 
    (cases program pgm 
      (a-program (exp1) 
        (value-of-command exp1 env)))))
;;;;;;;;;;;;;;;;;;

;evaluate a command
(define value-of-command
  (lambda(com env)
    (cases command com
      (single-command (exp1)
        (value-of-keyword exp1 env))
      (multi-command (exp1 exp2)
        (let ([environment (value-of-program exp1 env)])
          (if (env? environment)
            (value-of-keyword exp2 environment) (value-of-keyword exp2 env))
          )))))
;;;;;;;;;;;;;;;;

;evaluate a keyword
(define value-of-keyword
  (lambda(exp env)
    (cases keyword exp
      (return (exp1)
        (display (value-of-expression exp1 env)))
      (assignment_statement (var exp1)
        (extend-env var (value-of-expression exp1 env) env))
      (if_statement (exp pgm1 pgm2)
        (if (equal? (value-of-expression exp env) #t)
          (value-of-program pgm1 env)
          (value-of-program pgm2 env)))
      (while_statement (exp pgm)
        (define a (equal? (value-of-expression exp env) #t))
        (if  a (value-of-keyword (while_statement exp pgm)  (value-of-program pgm env))  env)))))
;;;;;;;;;;;;;;;;;;;;;;
;evaluate expression
(define value-of-expression
  (lambda(exp env)
    (cases expression exp
      (aexpression (exp1)
        (value-of-aexpression exp1 env))
      (grt (exp1 exp2)
        (let ([val1 (value-of-aexpression exp1 env)] [val2 (value-of-aexpression exp2 env)])
          (let ([type-val1 (type-check val1)] [type-val2 (type-check val2)])
          (cases expval type-val1
            (num-val (num) 
              (cases expval type-val2
                (num-val (num) (if (> val1 val2) #t #f))
                (string-val (str) (raise "'>' not supported between instances of 'num' and 'str'"))
                (else (raise "unsupported type(s) for '>'"))))
            (string-val (str)
              (cases expval type-val2
                (num-val (num) (raise "'>' not supported between instances of 'str' and 'num'"))
                (string-val (str) (if (string>? val1 val2) #t #f))
                (else (raise "unsupported type(s) for '>'"))))
            (list-val (l)
              (cases expval type-val2
                (list-val (l) (raise " '>' not supported for instances of list"))
                (else (raise "unsupported type(s) for '>'"))))
            (else 'error)))))
      (les (exp1 exp2)
        (let ([val1 (value-of-aexpression exp1 env)] [val2 (value-of-aexpression exp2 env)])
          (let ([type-val1 (type-check val1)] [type-val2 (type-check val2)])
          (cases expval type-val1
            (num-val (num) 
              (cases expval type-val2
                (num-val (num) (if (< val1 val2) #t #f))
                (string-val (str) (raise "'<' not supported between instances of 'num' and 'str'"))
                (else (raise "unsupported type(s) for '<'"))))
            (string-val (str)
              (cases expval type-val2
                (num-val (num) (raise "'<' not supported between instances of 'str' and 'num'"))
                (string-val (str) (if (string<? val1 val2) #t #f))
                (else (raise "unsupported type(s) for '<'"))))
            (list-val (l)
              (cases expval type-val2
                (list-val (l) (raise " '>' not supported for instances of list"))
                (else (raise "unsupported type(s) for '<'"))))
            (else 'error)))))
      (eqq (exp1 exp2)
        (let ([val1 (value-of-aexpression exp1 env)] [val2 (value-of-aexpression exp2 env)])
          (let ([type-val1 (type-check val1)] [type-val2 (type-check val2)])
            (cases expval type-val1
              (num-val (num)
                (cases expval type-val2
                  (num-val (num) (if (equal? val1 val2) #t #f))
                  (else #f)))
              (string-val (str)
                (cases expval type-val2
                  (string-val (str) (if (string=? val1 val2) #t #f))
                  (else #f)))
              (list-val (l)
                (cases expval type-val2
                  (list-val (l) (if (equal? (length val1) (length val2)) (if (equal? val1 val2) #t #f) #f))
                  (else #f)))
              (bool-val (bool)
                (cases expval type-val2
                  (bool-val (bool) (if (equal? val1 val2) #t #f))
                  (else #f)))
              (else #f)))))

      (neq (exp1 exp2)
        (let ([val1 (value-of-aexpression exp1 env)] [val2 (value-of-aexpression exp2 env)])
          (let ([type-val1 (type-check val1)] [type-val2 (type-check val2)])
            (cases expval type-val1
              (num-val (num)
                (cases expval type-val2
                  (num-val (num) (if (equal? val1 val2) #f #t))
                  (else #t)))
              (string-val (str)
                (cases expval type-val2
                  (string-val (str) (if (string=? val1 val2) #f #t))
                  (else #t)))
              (list-val (l)
                (cases expval type-val2
                  (list-val (l) (if (equal? (length val1) (length val2)) (if (equal? val1 val2) #f #t) #t))
                  (else #t)))
              (bool-val (bool)
                (cases expval type-val2
                  (bool-val (bool) (if (equal? val1 val2) #f #t))
                  (else #t)))
              (else #t))))))))
;;;;;;;;;;;;;;;;;;;;

;evaluate aexp
(define value-of-aexpression
  (lambda(exp env)
    (cases aexp exp
      (bexpression (exp1)
        (value-of-bexpression exp1 env))
      (plus (exp1 exp2)
            (let ([val1 (value-of-bexpression exp1 env)] [val2 (value-of-aexpression exp2 env)])
              (let ([type-val1 (type-check val1)] [type-val2 (type-check val2)])
                (cases expval type-val1
                  (num-val (num1)
                           (cases expval type-val2
                             (num-val (num2) (+ num1 num2))
                             (list-val (l)
                                       (if (andmap number? l) (map (lambda (listnum) (+ num1 listnum)) l) (raise "unsupported operand type(s) for + : number and non-number list")))
                             (else (raise "unsupported operand type(s) for +"))))
                  (list-val (l1)
                            (cases expval type-val2
                              (num-val (num1)
                                       (if (andmap number? l1) (map (lambda (listnum) (+ num1 listnum)) l1) (raise "unsupported operand type(s) for + : non-number list and number")))
                              (list-val (l2) (append l1 l2))
                              (bool-val (bool1)
                                        (if (andmap boolean? l1) (map (lambda (listbool) (or bool1 listbool)) l1) (raise "unsupported operand type(s) for or: non-bool list and bool")))
                              (string-val (str1)
                                          (if (andmap string? l1) (map (lambda (liststr) (string-append liststr str1)) l1) (raise "unsupported operand type(s) for +: non-string list and string")))
                              (else (raise "unsupported operand type(s) for +"))))
                  (bool-val (bool1)
                            (cases expval type-val2
                              (list-val (l1)
                                        (if (andmap boolean? l1) (map (lambda (listbool) (or bool1 listbool)) l1) (raise "unsupported operand type(s) for or: bool and non-bool list")))
                              (else (raise "unsupported operand type(s) for +"))))
                  (string-val (str1)
                              (cases expval type-val2
                                (string-val (str2) (string-append str1 str2))
                                (list-val (l1)
                                          (if (andmap string? l1) (map (lambda (liststr) (string-append str1 liststr)) l1) (raise "unsupported operand type(s) for +: string and non-string list")))
                                (else (raise "unsupported operand type(s) for +"))))
                  (else (raise "unsupported operand type(s) for +"))))))
      (minus (exp1 exp2)
             (let ([val1 (value-of-bexpression exp1 env)] [val2 (value-of-aexpression exp2 env)])
               (let ([type-val1 (type-check val1)] [type-val2 (type-check val2)])
                 (cases expval type-val1
                   (num-val (num1)
                            (cases expval type-val2
                              (num-val (num2) (- num1 num2))
                              (list-val (l)
                                        (if (andmap number? l) (map (lambda (listnum) (- num1 listnum)) l) (raise "unsupported operand type(s) for - : number and non-number list")))
                              (else (raise "unsupported operand type(s) for -"))))
                   (list-val (l1)
                             (cases expval type-val2
                               (num-val (num1)
                                        (if (andmap number? l1) (map (lambda (listnum) (- num1 listnum)) l1) (raise "unsupported operand type(s) for - : non-number list and number")))
                               (else (raise "unsupported operand type(s) for -"))))
                   (else (raise "unsupported operand type(s) for -")))))))))
;;;;;;;;;;;;;;;;

; evaluate bexp
(define value-of-bexpression
  (lambda(exp env)
    (cases bexp exp
      (cexpression (exp1)
        (value-of-cexpression exp1 env))
      (mul (exp1 exp2)
            (let ([val1 (value-of-cexpression exp1 env)] [val2 (value-of-bexpression exp2 env)])
              (let ([type-val1 (type-check val1)] [type-val2 (type-check val2)])
                (cases expval type-val1
                  (num-val (num1)
                           (cases expval type-val2
                             (num-val (num2) (* num1 num2))
                             (list-val (l)
                                       (if (andmap number? l) (map (lambda (listnum) (* num1 listnum)) l) (raise "unsupported operand type(s) for * : number and non-number list")))
                             (else (raise "unsupported type(s) for *"))))
                  (list-val (l1)
                            (cases expval type-val2
                              (num-val (num1)
                                       (if (andmap number? l1) (map (lambda (listnum) (* num1 listnum)) l1) (raise "unsupported operand type(s) for * : non-number list and number")))
                              (bool-val (bool1)
                                        (if (andmap boolean? l1) (map (lambda (listbool) (and bool1 listbool)) l1) (raise "unsupported operand type(s) for and : non-bool list and bool")))            
                              (else (raise "unsupported type(s) for *"))))
                  (bool-val (bool1)
                            (cases expval type-val2
                              (list-val (l1)
                                        (if (andmap boolean? l1) (map (lambda (listbool) (and bool1 listbool)) l1) (raise "unsupported operand type(s) for and : bool and non-bool list")))
                              (else (raise "unsupported type(s) for *"))))
                  (else (raise "unsupported type(s) for *"))))))

      (div (exp1 exp2)
            (let ([val1 (value-of-cexpression exp1 env)] [val2 (value-of-bexpression exp2 env)])
              (let ([type-val1 (type-check val1)] [type-val2 (type-check val2)])
                (cases expval type-val1
                  (num-val (num1)
                           (cases expval type-val2
                             (num-val (num2) (if (zero? num2) (raise "Division by zero") (/ num1 num2)))
                             (list-val (l)
                                       (if (andmap number? l) (if (ormap zero? l) (raise "Division by zero") (map (lambda (listnum) (/ num1 listnum)) l)) (raise "unsupported operand type(s) for / : number and non-number list")))
                             (else (raise "unsupported type(s) for /"))))
                  (list-val (l1)
                            (cases expval type-val2
                              (num-val (num1)
                                       (if (andmap number? l1) (if (zero? num1) (raise "Division by zero") (map (lambda (listnum) (/ listnum num1)) l1)) (raise "unsupported operand type(s) for / : non-number list and number")))            
                              (else (raise "unsupported type(s) for /"))))
                  (else (raise "unsupported type(s) for /")))))))))


;;;;;;;;;;;;;;;;;;;;

;evaluate cexp
(define value-of-cexpression
  (lambda(exp env)
    (cases cexp exp
      (neg (exp1)
           (let ([val1 (value-of-cexpression exp1 env)])
             (let ([type-val1 (type-check val1)])
               (cases expval type-val1
                 (num-val (num) (- num))
                 (bool-val (bool) (not bool))
                 (list-val (l)
                           (if (andmap number? l) (map - l) (if (andmap boolean? l) (map not l) (raise "unsupported type(s) for - (neg) : non-number list"))))
                 (else (raise "unsupported type(s) for - (neg)"))))))
      (number (num)
        num)
      (null () void) ; is void a good choice?
      (string (str)
        str)
      (vari (var)
         (apply-env env var))
      (boolean (bool)
               bool)
      (mk-list (l)
               (value-of-mylist l env))
      (indexing (var index)
                (value-of-listmember index env (apply-env env var)))
      (else (display ''mehrdad:/)))))
;;;;;;;;;;;;;;;;;;;;;;;;;



(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (string-val
   (str string?))
  (null-val)
  (list-val
   (l list?)))

(define type-check
  (lambda (exp)
    (cond
      [(number? exp) (num-val exp)]
      [(boolean? exp) (bool-val exp)]
      [(string? exp) (string-val exp)]
      [(void? exp) (null-val)]
      [(list? exp) (list-val exp)])))

(define value-of-mylist
  (lambda (exp env)
    (cases mylist exp
      (empty-list () '())
      (a-list (listvals) (value-of-listValues listvals env)))))

(define value-of-listValues
  (lambda (exp env)
    (cases listValues exp
      (oneelement (exp1)
                  (list (value-of-expression exp1 env)))
      (nelement (exp1 values)
                (cons (value-of-expression exp1 env) (value-of-listValues values env))))))

(define value-of-listmember
  (lambda (exp env the-list)
    (if (not (list? the-list)) (raise "Int object is not subscriptable") (let ([len-the-list (length the-list)]) 
    (cases listmember exp
      (oned (exp1)
            (let ([target (value-of-expression exp1 env)])
              (if (number? target) (if (> target (- len-the-list 1)) (raise "Array out of bound") (if (< target 0) (raise "Array out of bound") (list-ref the-list target))) (raise "List Indices must be integers"))))
      (nd (exp1 exp2)
          (let ([first-index (value-of-expression exp1 env)])
             (if (> first-index (- len-the-list 1)) (raise "Array out of bound") (if (< first-index 0) (raise "Array out of bound") (let ([new-list (list-ref the-list first-index)])
               (value-of-listmember exp2 env new-list)))))))))))


;test


(define lex-this (lambda (lexer input) (lambda () (lexer input))))

;;;;;;;;;;;;;;;;;;;;
(define evaluate
  (lambda (fileinp)
  (let ([inp (apply string-append (file->lines fileinp))])
    (let ([my-lexer (lex-this simple-math-lexer (open-input-string inp))])
      (value-of-program (let((parser-res (simple-math-parser my-lexer))) parser-res) (init-env))))))

(evaluate "c.txt")
















