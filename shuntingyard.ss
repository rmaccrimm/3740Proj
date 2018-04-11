#lang racket
(require racket/format)
(require racket/string)
(require readline/readline)

; Shunting-yard and RPN algorithm

; based on - https://github.com/osoleve/Scheme-Infix-Calculator/blob/master/shunting-yard.scm
; based on - https://rosettacode.org/wiki/Parsing/RPN_calculator_algorithm#Racket

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

(provide calculate)