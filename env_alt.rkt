#lang eopl

(provide
 ; build
 empty-env
 extend-env
 ; query
 apply-env)

;<env-rep> ::= (empty-env)
;              ()
;              ::= (extend-env ({<symbol>}*) ({<value>}*) <env-rep>)
;              (({<symbol>}*) ({<value>}*)) . <env-rep>)

(define (empty-env)
  '())

(define (extend-env syms vals env)
  (cons (list syms vals) env))

(define (apply-env env sym)
  (if (null? env)
      (eopl:error 'apply-env "No binding for ~s" sym)
      (let ((syms (car (car env)))
            (vals (cadr (car env)))
            (env (cdr env)))
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
              (list-ref vals pos)
              (apply-env env sym))))))

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
