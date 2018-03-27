#lang racket

(define (float? x)
  (and (not (integer? x)) (real? x)))

(define (def_value t)
  (cond ((string=? "%b" t) #f)
        ((string=? "%i" t) 0)
        ((string=? "%f" t) 0)))

(define (declare vars k t)
  ;; Ask Zhang this
  (cond ((null? vars)
         (list (list k (def_value t) t)))
        (else
         (if (string=? (car (car vars)) k)
             (append (list (list k (def_value t) t)) (cdr vars))
             (append (list (car vars)) (declare (cdr vars) k t))))))

  

(define (lookup vars k)
  (cond ((null? vars)
      '())
        (else
         (if (string=? k (car (car vars)))
             (car vars)
             (lookup (cdr vars) k)))))

(define (assign vars k v t)
  (cond ((null? vars)
        '())
        (else
         (if (string=? (car (car vars)) k)
             (if (string=? (car (cddr (car vars))) t)
                 (append (list (list k v t)) (cdr vars))
                 '())
             (append (list (car vars)) (assign (cdr vars) k v t))))))

(define vars '(("a" 123 "%i") ("b" #t "%b" ) ("c" 3.333 "%f")))

