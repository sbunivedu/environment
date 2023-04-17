#lang racket

(require "./env_ast.rkt")
(require rackunit)

(define dxy-env
  (extend-env '(d x) '(6 7)
              (extend-env '(y) '(8)
                          (empty-env))))

(check-eq? (apply-env dxy-env 'x) 7)
