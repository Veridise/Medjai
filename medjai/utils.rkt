#lang rosette
(provide (all-defined-out))

; helper for (not (zero? p))
(define (false? p)
  (zero? p))
(define (true? p)
  (! (false? p)))
(define (shift-left n m)
  (arithmetic-shift n m)) ; Python's version <<
(define (shift-right n m)
  (arithmetic-shift n (- m))) ; Python's version >>
