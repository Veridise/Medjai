#lang rosette

(require (only-in rosette/base/core/polymorphic ite))

(provide symint resume-vc get-all-syms
         ?complete-solution complete-evaluate
         rewrite-ite)

(define all-sym-vars empty)

(define (get-all-syms)
  (if (union? all-sym-vars)
    (symbolics (union-contents all-sym-vars))
    (symbolics all-sym-vars)))

(define (symint [prime #f])
  (define-symbolic* x integer?)
  (set! all-sym-vars (cons x all-sym-vars))
  (let ([sym x])
    (cond 
    [prime (begin (assume (>= x 0)) (assume (< x prime)) sym)]
    [else sym])))

(define (resume-vc my-vc)
  (assume (vc-assumes my-vc))
  (assert (vc-asserts my-vc)))

(define (?complete-solution sol vars)
  (if (unsat? sol)
    sol
    (complete-solution sol vars)))

(define (complete-evaluate val sol)
  (evaluate val (complete-solution sol (symbolics val))))

(define (rewrite-ite value)
  (match* (value)
    [((expression (== ite) a b c))
     (ite a (rewrite-ite b) (rewrite-ite c))]
    [((expression (== modulo) (expression (== ite) a b c) d))
     (ite a (rewrite-ite (modulo b d)) (rewrite-ite (modulo c d)))]
    [((expression (== +) (expression (== ite) a b c) d))
     (ite a (rewrite-ite (+ b d)) (rewrite-ite (+ c d)))]
    [((expression (== -) (expression (== ite) a b c) d))
     (ite a (rewrite-ite (- b d)) (rewrite-ite (- c d)))]
    [((expression (== +) d (expression (== ite) a b c)))
     (ite a (rewrite-ite (+ d b)) (rewrite-ite (+ d c)))]
    [((expression (== -) d (expression (== ite) a b c)))
     (ite a (rewrite-ite (- d b)) (rewrite-ite (- d c)))]
    [((expression (== -) (expression (== ite) a b c)))
     (ite a (rewrite-ite (- b)) (rewrite-ite (- c)))]
    [((expression (== -) (expression (== +) a b)))
     (rewrite-ite (+ (rewrite-ite (- a)) (rewrite-ite (- b))))]
    [((expression (== modulo) (expression (== modulo) a b) b))
     (rewrite-ite (modulo a b))]
    [(_) value]))