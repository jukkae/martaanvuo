#lang at-exp racket

(require reprovide/reprovide)
(require reprovide/require-transformer/glob-in)

(reprovide
  "dev-note.rkt"
  "dice.rkt"
  "list-utils.rkt"
  "menu-utils.rkt"
  "output.rkt"
  "string-utils.rkt")

(provide (all-defined-out))


; (: sexp? (-> Any Boolean : #:+ Sexp))
(define (sexp? s)
  (or (number? s)
      (symbol? s)
      (string? s)
      (and (pair? s) (sexp? (car s)) (sexp? (cdr s)))
      (null? s)))

(define-syntax append-element!
  (syntax-rules ()
    ((_ lst elem) (begin (set! lst (append-element lst elem)) lst))))

(define-syntax append!
  (syntax-rules ()
    ((_ lst1 lst2) (begin (set! lst1 (append lst1 lst2)) lst1))))
