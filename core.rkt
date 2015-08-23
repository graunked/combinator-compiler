#lang racket
(require "lambda-to-combinator.rkt")
(require "ski-compiler.rkt")

(define (run expr param)
  (evaluate (ltc `(,expr ,param))))

(define (desugar expr)
  (match expr
    [`(λ (,x) ,body) `(λ (,x) ,(desugar body))]
    [`(let ((,v ,exp) ...) ,body) (desugar `((λ (,@v) ,body) ,@exp))]
    [`(letrec [(,f ,lam)] ,body)  (desugar `(let ((,f (Y (λ (,f) ,lam)))) ,body))]
    [_ expr]))

(define factorial
  '(λ (x)
     (letrec [(fac (λ (y) 
                     (((cond ((= y) 0))
                           1)
                           ((mult y) (fac ((- n) 1))))))]
       (fac x))))

(define (test)
  (run (ltc (desugar factorial)) 5))