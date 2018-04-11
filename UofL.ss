#lang racket
(require "shuntingyard.ss")

; the interface for if statements e.x. "if (1 == 1) then b = 9 endif"
(define (select-if string vars)
  (if (regexp-match #rx"elseif" string)
      (if2 string vars)  ; does the 2 if stmt case
      (if1 string vars)) ; does the single if stmt case
  )

; single if statement
(define (if1 string vars)
  (println string)
  (let ([condition (car (cdr (regexp-match #rx"\\((.*)\\)" string)))]
        [string-stmt (car (cdr (regexp-match #rx"then(.*)endif" string)))])
    (cond ((calculate (substitute vars (tokenize condition)))
           (op string-stmt vars))
          (else
           vars))))

; two if statements
(define (if2 string vars)
  (let* ([condition1 (car (regexp-match* #rx"[^\\(\\)]*(?<=\\()[^\\)]*(?=\\))[^\\(\\)]*" string))]
         [string-stmt1 (car (regexp-match* #rx"(?<=then)(.*?)(?=elseif|endif)" string))]
         [condition2 (cadr (regexp-match* #rx"[^\\(\\)]*(?<=\\()[^\\)]*(?=\\))[^\\(\\)]*" string))]
         [string-stmt2 (cadr (regexp-match* #rx"(?<=then)(.*?)(?=elseif|endif)" string))])
    (cond ((calculate (substitute vars (tokenize condition1))) ;; The first true condition
           (op string-stmt1 vars))  ;;TODO: place the string to stmt parsing function to evaluate the string
          ((calculate (substitute vars (tokenize condition2))) ;; The second true condition
           (op string-stmt2 vars))  ;;TODO: place the string to stmt parsing function to evaluate the string
          (else
           vars))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define rx_identifier #rx"(?:(?:[a-zA-Z_][0-9a-zA-Z_]+)|(?:[a-zA-HK-Z]))")
(define rx_assign #rx"^ *((?:(?:[a-zA-Z_][0-9a-zA-Z_]+)|(?:[a-zA-HK-Z]))) *=(?!=)(.*)")
(define rx_define #rx"^ *#definevari +((?:(?:[a-zA-Z_][0-9a-zA-Z_]+)|(?:[a-zA-HK-Z]))) +(integer|boolean|float)")
(define rx_if #rx"^ *if *\\(.*?\\) *then *$")
(define rx_if_full #rx"^ *if")
(define rx_for #px"^\\s*for.*do\\s*$")
(define rx_for_cap #px"^\\s*for\\s+(I|J)\\s*=\\s*(\\d+)\\s+to\\s+(\\d+)\\s+st\\s+(\\d+)\\s+do\\s*&(.*)")
(define rx_func #px"^\\s*#definefunc\\s+([a-zA-Z_]+[0-9a-zA-Z_]*)\\s+(.*)")
(define rx_params #rx"((?:(?:[a-zA-Z_][0-9a-zA-Z_]+)|(?:[a-zA-HK-Z])))\\((.*)\\)")
(define true "(1 == 1)")
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


;;
(define (declare_func funcs name params stmts)
  (cond ((null? funcs)
         (list (list name params stmts)))
        (else
         (if (string=? (car (car funcs)) name)
             (append (list (list name params stmts)) (cdr funcs))
             (append (list (car funcs)) (declare_func (cdr funcs) name params stmts))))))
               


;; Find value of variable k in list vars, if it exists. Otherwise return
;; empty list
(define (lookup vars k)
  (cond ((null? vars)
      '())
        (else
         (if (string=? k (car (car vars)))
             (car vars)
             (lookup (cdr vars) k)))))


;; Cast value v to the given type
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
        (else
         (let ((retval (substitute vars (cdr tokens))))
           (if (and (pair? (cdr tokens)) (null? retval))
               '()
               (cond ((and (false? (regexp-match rx_identifier (car tokens)))
                          (not (regexp-match? #rx"(I|J)" (car tokens)))) ; Not an identifier
                      (append (list (car tokens)) (substitute vars (cdr tokens))))
                     (else ; is an identifier
                      (let ((look (lookup vars (car tokens))))
                        (if (null? look)
                            '()
                            (let ((l (get_i look 2)))
                              (cond ((boolean? l)
                                     (if (false? l)
                                         (set! l false)
                                         (set! l true)))
                                    (else
                                     (set! l (number->string l))))
                              (append (tokenize l) retval)))))))))))
                               

;; Read input until term is found
(define (read_next expr term count)
  (display "    > ")
  (let ((s (read-line)))
    (cond ((regexp-match? expr s)
           (set! count (+ 1 count)))
          ((regexp-match? (pregexp (string-append " *" term " *$")) s)
           (set! count (- count 1))))
    (cond ((= 0 count)
          (list s))
          (else
           (append (list s) (read_next expr term count))))))




;; no comment
(define (for_loop vars stmts ind stop step)
  (println ind)
  (let ((i (get_i (lookup vars ind) 2)))
    (cond ((> i stop)
           vars)
          (else
           (set! vars (stmt_loop vars stmts))
           (set! vars (assign vars ind (+ i step)))
           (for_loop vars stmts ind stop step)))))


;; Execute a list of statements by calling op
(define (stmt_loop vars stmts)
    (cond ((null? stmts)
           vars)
          (else
           (set! vars (op (car stmts) vars))
           (stmt_loop vars (cdr stmts)))))



;; Split the body of a for loop into statments
(define (parse_for input)
  (let (( stmts (regexp-match #px"\\s*(.*)(for.*?endfor)\\s*(.*)endfor" input))) ;; contains for loop
        (cond ((false? stmts)
               (cleanup (regexp-split #rx"&" (regexp-replace #rx"endfor" input ""))))
              (else
               (append (cleanup (regexp-split #rx"&" (get_i stmts 2)))
                        (list(get_i stmts 3))
                        (cleanup (regexp-split #rx"&" (get_i stmts 4))))))))


;;
(define (declare_all vars l v)
  (println l)
  (cond ((null? l)
         vars)
         (else
          (set! vars (declare vars (type_map (get_i l 2)) (get_i l 1)))
          (set! vars (assign vars (get_i l 1) (string->number (car v))))
          (declare_all vars (cddr l) (cdr v)))))
          
        

(define (ex_func vars name params)
  (set! vars (declare_all vars (get_i (lookup vars name) 2) params))
  (stmt_loop vars (get_i (lookup vars name) 3))
  vars)

;; Operations for every valid statement
(define (op input vars)
  (println input)
  (cond ((pair? (regexp-match #rx"^ *#definevari *" input))
         ;; Declare variable
         (let ((s (regexp-match rx_define input)))
           (cond ((boolean? s)
                     (println "Error: bad arguments for #definevari")
                     vars)
                   (else 
                     (declare vars (type_map (get_i s 3)) (get_i s 2))))))
        ;; Define function
        ((pair? (regexp-match #px"^\\s*#definefunc\\s*" input))
         (let ((s (regexp-match rx_func input)))
           (println s)
           (declare_func vars (get_i s 2) (regexp-split #rx" " (get_i s 3))
                         (func_loop '()))))
                       
        ;; Assignment
        ((pair? (regexp-match rx_assign input))
         (let ((s (regexp-match rx_assign input)))
           (let ((retval (calculate (substitute vars (tokenize (get_i s 3))))))
             (assign vars (get_i s 2) retval))))
        ;; If statement
        ((regexp-match? rx_if_full input)
         (select-if input vars))
        ;; For loops
        ((regexp-match? rx_for_cap input)
         (let ((fvars (regexp-match rx_for_cap input)))
           (set! vars (declare vars "%i" (get_i fvars 2)))
           (set! vars (assign vars (get_i fvars 2) (string->number (get_i fvars 3))))
           (set! vars (for_loop vars
                                (parse_for (get_i fvars 6))
                                (get_i fvars 2) ; index
                                (string->number (get_i fvars 4))   ; stop
                                (string->number (get_i fvars 5)))) ; stepsize
           vars))
        ;; Execute function
        ((regexp-match? rx_params input)
         (let ((s (regexp-match rx_params input)))
           (ex_func vars (get_i s 2) (regexp-split #rx" " (get_i s 3)))))
         
        ;; Immediate
        (else
         (let ((result (substitute vars (tokenize input))))
           (if (null? result)
               (println "Invalid identifier given")
               (print (calculate result)))) 
         vars))) 

(define (func_loop stmts)
  (display "   > ")
  (let ((s (read-line)))
    ;(println s)
    (cond ((not (string=? s "#definefunc"))
           (cond ((regexp-match? rx_if s)
                  (set! stmts (append stmts (list (string-join (append (list s) (read_next rx_if "endif" 1)) " ")))))
                 ((regexp-match? rx_for s)
                  (set! stmts (append stmts (list (string-join (append (list s) (read_next rx_for "endfor" 1)) "&")))))
                 (else
                  (set! stmts (append stmts (list s)))))
           (func_loop stmts))
          (else
            stmts))))

;; Get input
(define (main_loop vars)
  (display "UofL> ")
  (let ((s (read-line)))
    ;(println s)
    (cond ((not (string=? s "#exit"))
           (cond ((regexp-match? rx_if s)
                  (set! vars
                        (op (string-join (append (list s) (read_next rx_if "endif" 1)) " ")
                            vars)))
                 ((regexp-match? rx_for s)
                  (set! vars
                        (op (string-join (append (list s) (read_next rx_for "endfor" 1)) "&")
                            vars)))
                 (else
                  (set! vars (op s vars))))
           (println vars)
           (main_loop vars)))))


(define vars '(("a" 123 "%i") ("b" #t "%b" ) ("c" 3.333 "%f")))
(define funcs (list (list "func1" (list "p1" "integer" "p2" "integer") (list "a" "c+10.3"))))

(define (uofl)
  (main_loop vars))