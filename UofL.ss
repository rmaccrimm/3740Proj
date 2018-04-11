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


; the interface for the for loop e.x "for I = 1 to 10 stepsize 2 do "statements" endfor"
(define (for-loop string vars)
  (let* ([initial (string->number (car (regexp-match* #px"[[:digit:]]+" string)))]
         [final (string->number (car (cdr (regexp-match* #px"[[:digit:]]+" string))))]
         [stepsize (string->number (car (cdr (cdr (regexp-match* #px"[[:digit:]]+" string)))))]
         [loop-stmt (car (regexp-match* #rx"(?<=do)(.*)(?=endfor)" string))])
    (do-for-loop initial final stepsize loop-stmt)))

; the final is inclusive
(define (do-for-loop initial final stepsize loop-stmt)
  (if (not (> initial final))
      (begin (printf loop-stmt)(newline)
             (do-for-loop (+ initial stepsize) final stepsize loop-stmt))
      (print"loop ended")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define rx_identifier #rx"(?:(?:[a-zA-Z_][0-9a-zA-Z_]+)|(?:[a-zA-HK-Z]))")
(define rx_assign #rx"^ *((?:(?:[a-zA-Z_][0-9a-zA-Z_]+)|(?:[a-zA-HK-Z]))) *=(?!=)(.*)")
(define rx_define #rx"^ *#definevari +((?:(?:[a-zA-Z_][0-9a-zA-Z_]+)|(?:[a-zA-HK-Z]))) +(integer|boolean|float)")
(define rx_if #rx"^ *if *\\(.*?\\) *then *$")
(define rx_if_full #rx"^ *if")
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
               (cond ((false? (regexp-match rx_identifier (car tokens))) ; Not an identifier
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
(define (read_next term)
  (display "    > ")
  (let ((s (read-line)))
    (if (regexp-match? (pregexp (string-append " *" term " *$")) s)
        (list s)
        (append (list s) (read_next term))))) 

;(define (do-stats l))
  

;(define (loop stats stop stepsize)
 ; do all stats with op
  ;(loop)

;(define (for_loop vars string))
  

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
        ;; Assignment
        ((pair? (regexp-match rx_assign input))
         (let ((s (regexp-match rx_assign input)))
           (let ((retval (calculate (substitute vars (tokenize (get_i s 3))))))
             (assign vars (get_i s 2) retval))))
        ;; If statement
        ((regexp-match? rx_if_full input)
         (select-if input vars))
        ;; For loops
        
        (else
         (let ((result (substitute vars (tokenize input))))
           (if (null? result)
               (println "Invalid identifier given")
               (println (calculate result))))
         vars)))


;; Get input
(define (main_loop vars)
  (display "UofL> ")
  (let ((s (read-line)))
    (println s)
    (cond ((not (string=? s "#exit"))
           (cond ((regexp-match? rx_if s)
               (set! vars
                     (op
                      (string-join (append (list s) (read_next "endif")) " ")
                      vars)))
                 (else
                  (set! vars (op s vars))))
           (println vars)
           (main_loop vars)))))


(define vars '(("a" 123 "%i") ("b" #t "%b" ) ("c" 3.333 "%f")))

(define (uofl)
  (main_loop vars))