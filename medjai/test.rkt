#lang rosette

(struct func (f) #:transparent #:mutable)

(define a (func (lambda (f) #f)))

(define (set i v)
  (let ([af (func-f a)])
    (set-func-f! a (lambda (j) (if (equal? i j) v (af j))))))

(define (get i) ((func-f a) i))

(get 1)
(set 1 2)
(get 1)
(set 3 4)
(get 1)

(define-symbolic b c boolean?)
(define-symbolic x y integer?)

(if b (set 3 5) (set 4 5))
(get 3)

(set x 9)
(get 3)