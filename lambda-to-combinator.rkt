#lang racket

(define (lambda-to-combinators expr)
  (match expr
    (`(λ (,x) (λ (,y) ,z)) (lambda-to-combinators `(λ (,x) ,(lambda-to-combinators `(λ (,y) ,z)))))
    (`(λ (,x) (,y . ,x) ) (lambda-to-combinators y))
    (`(λ (,x) (,e1 . ,e2)) `((S . ,(lambda-to-combinators `(λ (,x) ,e1))) .
                                                      ,(lambda-to-combinators `(λ (,x) ,e2))))
    (`(λ (,x) ,x) 'I)
    (`(λ (,x) ,y) `(K . ,(lambda-to-combinators y)))
    (`(,e1 . ,e2) `(,(lambda-to-combinators e1) . ,(lambda-to-combinators e2)))
    (_ expr)))
  

(define ltc lambda-to-combinators)

(define compose '(λ (f) (λ (g) (λ (x) (f . (g . x))))))
(define bad-compose '(K (((S (K f)) (((S (K g)) I))))))

(define loop '(λ (x) (λ (y) (x y))))
(define succ '(λ (n) (λ (f) (f (n f)))))

(define (test) (ltc compose))

(provide ltc lambda-to-combinators)