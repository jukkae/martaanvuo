#lang at-exp racket

(provide (all-defined-out))

; dice shorthand
(define (d n sides)
  (for/sum ([i n]) (add1 (random sides))))
