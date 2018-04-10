#lang racket
(require racket/format)
(require racket/string)
(require readline/readline)

; Shunting-yard and RPN algorithm
; Based on code by ... from https://github.com/...

; Operator list
(define (operator? symbol)
  (member symbol '("+" "-" "*" "/" "%" "^" "==" "<>" ">=" "<=" ">" "<"))
  )

; Determines the associativitiy of a given mathematical operator
(define (asso-of operator)
  (if (member operator '("+" "-" "*" "/"))
      'left
      'right
      )
  )

; Determine the precedence of a given mathematical operator
(define (prec-of operator)
  (case operator
    (("==" "<>" "<=" ">=" "<" ">") 1)
    (("+" "-")                     2)
    (("*" "/")                     3)
    (("^")                         4)
    (else                          0)
    )
  )

; Action to take if the token in the stmt is an operator
(define (operator-actions stmt stack)
  (let* ([token-prec (prec-of (car stmt))]
         [token-asso (asso-of (car stmt))]
         [stack-oper (if (not (null? stack))
                         (car stack)
                         '())]
         [stack-prec (if (not (null? stack-oper))
                         (prec-of stack-oper)
                         0)])
    (cond ((or (and (eq? token-asso 'left)
                    (<= token-prec stack-prec))
               (and (eq? token-asso 'right)
                    (< token-prec stack-prec)))
           (cons stack-oper (shunting-yard stmt (cdr stack))))
          (else (shunting-yard (cdr stmt)(cons (car stmt) stack))))
    )
  )

; Action to take if (null? stmt)
(define (stack-op stack)
  (cond ((and (not (null? stack))
              (equal? (car stack) "("))
         (display "unbalenced paranthesis"))
        ((null? stack) '())
        (else (cons (car stack)(shunting-yard '() (cdr stack)))))
  )

; Main body for shunting-yard
(define (shunting-yard stmt stack)
  (cond ((null? stmt)
         (stack-op stack))
        ((number? (string->number (car stmt)))
        (cons (car stmt) (shunting-yard (cdr stmt) stack)))
        ((operator? (car stmt))
         (operator-actions stmt stack))
        ((equal? (car stmt) "(")
         (shunting-yard (cdr stmt)(cons (car stmt) stack)))
        ((equal? (car stmt) ")")
         (if (equal? "(" (car stack ))
             (shunting-yard (cdr stmt) (cdr stack))
             (cons (car stack )(shunting-yard stmt (cdr stack)))))
        )
  )
; Calculates the result from Reverse Polish Notation (The result from Shunting-Yard)
(define (RPN expr)
  (for/fold ([stack '()]) ([token expr])
    (match* (token stack)
     [((? number? n) s) (cons n s)]
     [('+ (list x y s ___)) (cons (+ x y) s)]
     [('- (list x y s ___)) (cons (- y x) s)]
     [('* (list x y s ___)) (cons (* x y) s)]
     [('/ (list x y s ___)) (cons (/ y x) s)]
     [('^ (list x y s ___)) (cons (expt y x) s)]
     [('== (list x y s ___)) (cons (eq? y x) s)]
     [('<> (list x y s ___)) (cons (not (eq? y x)) s)]
     [('<= (list x y s ___)) (cons (or (< y x)(eq? x y)) s)]
     [('>= (list x y s ___)) (cons (or (> y x)(eq? x y)) s)]
     [('> (list x y s ___)) (cons (> y x) s)]
     [('< (list x y s ___)) (cons (< y x) s)]
     [(x s) (error "calculate-RPN: Cannot calculate the expression:"
                   (reverse (cons x s)))])))

; Returns the result from infix notation.
; The input is the tokenized string of the statement.
; It will return a numerical value if the statement is an arithmatic operation
; or a boolean value if the statement is a logical operation.
(define (calculate stmt)
  (car (RPN (in-port read (open-input-string (string-join (shunting-yard stmt '()))))
  )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define rx_identifier "[a-zA-Z_]+[a-zA-Z_0-9]*")
(define rx_define #rx"^ *#definevari +([a-zA-Z_]+[a-zA-Z_0-9]*) +(integer|boolean|float)")
(define rx_assign #rx"^ *([a-zA-Z_]+[a-zA-Z_0-9]*) ?= ?([^=]+)")
(define rx_if #rx"^ *if *\\( *(.*[a-zA-Z_0-9]+) *\\) *then *\n")
;(define true (list "(" "1" "==" "1" ")"))
(define true "(1 == 1)")
;(define false (list "(" "1" "==" "0" ")"))
(define false "(1 == 0)")

(require "parsing.ss")

(define (float? x)
  (and (not (integer? x)) (real? x)))

;; Access ith elemetn in list l
(define (get_i l i)
  (if (= i 1)
      (car l)
      (get_i (cdr l) (- i 1))))

;; Default values
(define (def_value t)
  (cond ((string=? "%b" t) #f)
        ((string=? "%i" t) 0)
        ((string=? "%f" t) 0)))

;; Map type name to its representation
(define (type_map t)
  (cond ((string=? "integer" t) "%i")
        ((string=? "boolean" t) "%b")
        ((string=? "float" t) "%f")))

(define (type_string v)
  (cond ((boolean? v) "%b")
        ((exact-integer? v) "%i")
        ((real? v) "%f")))

;; Add a variable to list vars with type t and name (key) k.
;; Assign default value. If k already exists, re-declare it with new type
(define (declare vars t k)
  (cond ((null? vars)
         (list (list k (def_value t) t)))
        (else
         (if (string=? (car (car vars)) k)
             (append (list (list k (def_value t) t)) (cdr vars))
             (append (list (car vars)) (declare (cdr vars) t k))))))

;; Find value of variable k in list vars, if it exists. Otherwise return
;; empty list
(define (lookup vars k)
  (cond ((null? vars)
      '())
        (else
         (if (string=? k (car (car vars)))
             (car vars)
             (lookup (cdr vars) k)))))

;; 
(define (cast type v)
  (cond ((string=? type "%b")
         (if (boolean? v)
             v
             (not (zero? v))))
        ((string=? type "%i")
         (if (boolean? v)
             (if v
                 1
                 0)
             (inexact->exact (floor v))))               
        ((string=? type "%f")
         (if (boolean? v)
             (if v
                 1.0
                 0.0)
             (exact->inexact v)))))


;; Assign to k value v of type t. If t does not match k's type, return vars
;; unchanged.
(define (assign vars k v)
  (cond ((null? vars)
        '())
        (else
         (let ((t (get_i (car vars) 3)))
           (if (string=? (car (car vars)) k)
               (append (list (list k (cast t v) t)) (cdr vars))
               (append (list (car vars)) (assign (cdr vars) k v)))))))


;; Remove any empty strings
(define (cleanup l)
  (cond ((null? l)
         '())
        ((string=? (car l) "")
         (cleanup (cdr l)))
        (else
         (append (list (car l)) (cleanup (cdr l))))))
  


;; Split statement into list of identifiers/numbers/operators
(define (tokenize statement)
  (cleanup (regexp-split " +"
                (regexp-replace* #rx"(\\+|-|\\*|/|\\^|==|<>|<=|>=|<|>|\\(|\\))"
                                 statement " \\1 "))))



;; Replace identifiers with their stored value as a string
(define (substitute vars tokens)
  (cond ((null? tokens)
         '())
        ((string=? (car tokens) "false")
         (append (tokenize false) (substitute vars (cdr tokens))))
        ((string=? (car tokens) "true")
         (append (tokenize true) (substitute vars (cdr tokens))))
        ((false? (regexp-match "[a-zA-Z_][a-zA-Z_0-9]*" (car tokens))) ; is it an id
         (append (list (car tokens)) (substitute vars (cdr tokens))))
        (else
         (let ((l (get_i (lookup vars (car tokens)) 2)))
           (cond ((boolean? l)
                  (if (false? l)
                      (set! l false)
                      (set! l true)))
                 (else
                  (set! l (number->string l))))
           (append (tokenize l) (substitute vars (cdr tokens)))))))



;; Calculate a numeric or boolean value from a statement
;(define (arithmetic rhs)
  

;; Operations for every valid statement
(define (op input vars)
  (cond ((pair? (regexp-match #rx"^ *#definevari *" input))
         ;;declare variable
         (let ((s (regexp-match rx_define input)))
           (cond ((boolean? s)
                     (print "Error: bad arguments for #define\n")
                     vars)
                   (else 
                     (print "Here")
                   (declare vars (type_map (get_i s 3)) (get_i s 2)))
           )))
        ((pair? (regexp-match rx_assign input))
         (let ((s (regexp-match rx_assign input)))
           (let ((retval (calculate (substitute vars (tokenize (get_i s 3))))))
             (assign vars (get_i s 2) retval))))
         ;;assign a variable
        (else
         vars)
        ))


        
;;
(define (main_loop vars)
  (let ((s (read-line)))
    (print s)
    (cond ((not (string=? s "#exit"))
           (set! vars (op s vars))
           (display vars)
           (main_loop vars)))))
      
(define vars '(("a" 123 "%i") ("b" #t "%b" ) ("c" 3.333 "%f")))

