#lang racket

(define (assign vars k v)
  (cond ((null? vars)
        (list (list k v)))
        (else
         (if (string=? (car (car vars)) k)
             ((append (list (list k v)) (cdr vars)))
             ((append vars (assign (cdr vars) k)))))))

(define vars '(("a" 123) ("b" "Hello") ("c" 3.333)))

;;(define (set_pair vars k v)
 ;; (cond ((null? (lookup vars k))
  ;;       (append vars (list (list k v))))))
         

