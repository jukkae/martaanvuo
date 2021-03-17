#lang racket

; goto
(define-syntax label
  (syntax-rules ()
    ((_ name)
     (begin
       (define name)
       (call/cc (lambda (c) (set! name c)))))))

(define (goto label) (label))

; dice shorthand
(define (d n sides)
  (for/sum ([i n])
    (add1 (random sides))))

; list utils
(define (take-n-random l n)
  (take (shuffle l) n))

(define (slice l offset n)
  (take (drop l offset) n))

(define (take-random l)
  (list-ref l (random (length l))))

(define append-string string-append)

(provide (all-defined-out))