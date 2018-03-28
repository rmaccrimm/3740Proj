#lang racket

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
  ;; Ask Zhang this
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
(define (op input vars)
  (let ((s (regexp-split #rx" +" input)))
        (cond ((string=? (car s) "#definevari")
           (declare vars (type_map (get_i s 3)) (get_i s 2)))
          (else "no"))))

;;
(define (main_loop)
  (do ()
    (exit)
    ;; expressions
    (display "123")
    ))
      

(define vars '(("a" 123 "%i") ("b" #t "%b" ) ("c" 3.333 "%f")))

