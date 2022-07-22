#lang at-exp racket

(provide
  (all-defined-out)
  (all-from-out
    "dev-note.rkt"
    "dice.rkt"
    "list-utils.rkt"
    "menu-utils.rkt"
    "string-utils.rkt"))

(require
  "dev-note.rkt"
  "dice.rkt"
  "list-utils.rkt"
  "menu-utils.rkt"
  "string-utils.rkt")


; (: sexp? (-> Any Boolean : #:+ Sexp))
(define (sexp? s)
  (or (number? s)
      (symbol? s)
      (string? s)
      (and (pair? s) (sexp? (car s)) (sexp? (cdr s)))
      (null? s)))
