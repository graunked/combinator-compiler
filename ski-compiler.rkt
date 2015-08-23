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

(define (S spine)
  (let ([x (cdar spine)]
        [y (cdadr spine)]
        [z (cdaddr spine)])
    (cons (cons x z) (cons y z))))

(define (reduce [spine '()])
  (displayln spine)
  (match spine    
    [(list-rest 'S (cons _ x) (cons _ y) (cons _ z) w) (cons (cons (cons x z) (cons y z)) w)]
    [(list-rest 'K (cons _ x) (cons _ y) w)            (cons x w)]
    [(list-rest 'I (cons _ x) w)                       (cons x w)]
    [_ (cons (caar spine) spine)]))

(define (evaluate spine)
  (match spine
    [(list (? number? x)) spine]
    [_ (evaluate (reduce spine))]))

(define (evaluate2 expr)
  (let [[reduction (reduce expr)]]
    (displayln expr)
    (match reduction
      ['terminal expr]
      [`((,x ,y) ,z) (evaluate `(,(evaluate `(,x ,y)) ,z))]
      [`(,x (,y ,z)) `(,x ,(evaluate `(,y ,z)))]
      [_ (evaluate reduction)])))

(define (test) (evaluate '(((S I) I) x)))
(define (test2) (evaluate '((= 1) (I 1))))

(provide evaluate)



