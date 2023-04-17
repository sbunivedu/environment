#lang eopl

(provide
 ; build
 empty-env
 extend-env
 ; query
 apply-env)

;<env-rep> ::= (empty-env)
;          empty-env-record
;          ::= (extend-env ({<symbol>}*) ({<value>}*) <env-rep>)
;          extended-env-record (syms vals env)

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

(define (scheme-value? x)
  #t)

(define (empty-env)
  (empty-env-record))

(define (extend-env syms vals env)
  (extended-env-record syms vals env))

(define (apply-env env sym)
  (cases environment env
    (empty-env-record ()
                      (eopl:error 'apply-env "No binding for ~s" sym))
    (extended-env-record (syms vals env)
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
     (let ((list-index-r (list-index pred (cdr ls))))
       (if (number? list-index-r)
           (+ list-index-r 1)
           #f)))))

(define (list-of pred)
  (lambda (val)
    (or (null? val)
        (and (pair? val)
             (pred (car val))
             ((list-of pred) (cdr val))))))

