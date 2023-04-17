
#lang eopl

(provide
  ; build
  empty-env
  extend-env
  ; query
  apply-env)

(define (empty-env)
  (lambda (sym)
    (eopl:error 'apply-env "No binding for ~s" sym)))

(define (extend-env syms vals env)
  (lambda (sym)
    (let ((pos (list-find-position sym syms)))
      (if (number? pos)
          (list-ref vals pos)
          (apply-env env sym)))))

(define (apply-env env sym)
  (env sym))

(define (list-find-position sym los)
  (list-index (lambda (sym1) (eqv? sym1 sym)) los))

(define (list-index pred ls)
  (cond
    ((null? ls) #f)
    ((pred (car ls)) 0)
    (else
     (let ((list-index-r
            (list-index pred (cdr ls))))
       (if (number? list-index-r)
           (+ list-index-r 1)
           #f)))))
