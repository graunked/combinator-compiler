#lang racket
(require racket/match)

(define (reduce2 expr)
  (match expr
    [`(((S ,x) ,y) ,z) `((,x ,z) (,y ,z))]
    [`((K ,x) ,y) x]
    [`(Y ,h) `(,h (Y ,h))]
    [`(((C ,f) ,g) ,x) `((,f x) ,g)]
    [`(((B ,f) ,g), x) `(,f (,g ,x))]
    [`(I ,x) x]
    [`(U ,f (P ,x ,y)) `((,f ,x) ,y)]
    [`(((cond true) ,x) ,y) x]
    [`(((cond false) ,x) ,y) y]
    [`((plus ,x) ,y) #:when (and (number? x) (number? y)) (+ x y)]
    [`((plus ,x) ,y) `((plus ,(evaluate x)) ,(evaluate y))]
    [`((= ,x) ,y) #:when (and (number? x) (number? y)) (if (= x y) 'true 'false)]
    [`((= ,x) ,y) `((= ,(evaluate x)) ,(evaluate y))]
    [_ 'terminal]))

(define (reduce [spine '()])
  (displayln spine)
  (match spine
    [(list-rest (cons 'S x) (cons _ y) (cons _ z) tail) (cons (cons (cons x z) (cons y z)) tail)]
    [(list-rest (cons 'K x) (cons _ y) tail)            (cons x tail)]
    [(list-rest (cons 'I x) tail)                       (cons x tail)]
    
    [(list-rest (cons 'cond #t) (cons _ x) (cons _ y) w) (cons x w)]
    [(list-rest (cons 'cond #f) (cons _ x) (cons _ y) w) (cons y w)]
    
    [(list-rest (cons 'plus x) (cons _ y) w) (cons (+ (evaluate x) (evaluate y)) w)]
    [(list-rest (cons '= x) (cons _ y) w) (cons (= (evaluate x) (evaluate y)) w)]
    
    [(list-rest (? symbol? x) (cons _ y) z) (cons (cons x y) z)]
    [(list-rest (cons (? symbol? x) _) _) spine]
    
    [(list (cons head _) _ ...) (cons head spine)]
    [_ spine]))

(define (evaluate-spine spine)
  (let ([reduction (reduce spine)])
    (if (eq? reduction spine)
        spine
        (evaluate-spine reduction))))

(define (evaluate expr)
  (evaluate-spine (list expr)))

(define (test) (evaluate '(((S . I) . I) . x)))
(define (test2) (evaluate '((= . 1) . (I . 1))))

(provide evaluate)



