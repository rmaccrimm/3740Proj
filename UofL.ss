#lang racket

(define rx_define #rx"^ *#definevari +([a-zA-Z_]+[a-zA-Z_0-9]*) +(integer|boolean|float)")
(define rx_assign #rx"^ *([a-zA-Z_]+[a-zA-Z_0-9]*) ?= ?([^=]+)")
(define rx_if #rx"^ *if *\\( *(.*[a-zA-Z_0-9]+) *\\) *then *\n")
;;(regexp-match #rx"#define *[a-zA-Z]+(?<!i)" "#define tilslsthisi")

(define (float? x)
  (and (not (integer? x)) (real? x)))

(define (get_i l i)
  (if (= i 1)
      (car l)
      (get_i (cdr l) (- i 1))))

(define (def_value t)
  (cond ((string=? "%b" t) #f)
        ((string=? "%i" t) 0)
        ((string=? "%f" t) 0)))

(define (type_map t)
  (cond ((string=? "integer" t) "%i")
        ((string=? "boolean" t) "%b")
        ((string=? "float" t) "%f")))

(define (declare vars t k)
  (cond ((null? vars)
         (list (list k (def_value t) t)))
        (else
         (if (string=? (car (car vars)) k)
             (append (list (list k (def_value t) t)) (cdr vars))
             (append (list (car vars)) (declare (cdr vars) t k))))))

(define (lookup vars k)
  (cond ((null? vars)
      '())
        (else
         (if (string=? k (car (car vars)))
             (car vars)
             (lookup (cdr vars) k)))))

;;
(define (assign vars k v t)
  (cond ((null? vars)
        '())
        (else
         (if (string=? (car (car vars)) k)
             (if (string=? (car (cddr (car vars))) t)
                 (append (list (list k v t)) (cdr vars))
                 '())
             (append (list (car vars)) (assign (cdr vars) k v t))))))

;;
;;(define (op input vars)
;;  (set! input (regexp-replace #rx"([^=])=([^=])" input "\\1 = \\2"))
;;  (let ((s (regexp-split #rx" +" input)))
;;        (cond ((string=? (car s) "#definevari")
;;               (if (boolean? (regexp-match #rx"^[0-9]" (get_i s 2)))
;;                      (declare vars (type_map (get_i s 3)) (get_i s 2))
;;                      "Invalid identifier name"))
;;          (else
;;           (input)))))

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
        (else
         vars)
        ))
        ;;((boolean? (regexp-match #rx"^ *#clear *" input))
         
         ;;)
        ;;((boolean? (regexp-match #rx"^ *#exit *" input))

         ;;)
        ;;((boolean? (regexp-match #rx"^ * *" input))

         ;;)))

        
;;
(define (main_loop vars)
  (let ((s (read-line)))
    (cond ((not (string=? s "#exit"))
           (set! vars (op s vars))
           (display vars)
           (main_loop vars)))))

;;(define (main_loop vars c)
;;  (do ((it c (+ c 1)))
;;    ((= c -1) "Goodbye :)")
;;    (let ((s (read-line)))
   ;   (cond ((string=? s "#exit")
  ;           (print "here")
     ;        (break)))
    ;  (set! vars (op s vars))
     ; (display vars))
    ;))

      

(define vars '(("a" 123 "%i") ("b" #t "%b" ) ("c" 3.333 "%f")))

