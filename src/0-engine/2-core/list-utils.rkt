#lang at-exp racket

(provide (all-defined-out))

(require "dice.rkt")

(define (take-n-random l n)
  (take (shuffle l) n))

(define (slice l offset n)
  (take (drop l offset) n))

; distributions:
; 'constant: every value equally likely
; 'quadratic: every value half as likely as the previous one
; 'golden-ratio: every value phi times less likely as the previous one
(define (take-random l #:distribution [distribution 'constant])
  (case distribution
   ['constant
    (list-ref l (random (length l)))]
   ['quadratic
    (define n (length l))
    (define max_roll (- (expt 2 n) 1))
    (define roll (d 1 max_roll))
    (define index (- (- n (exact-floor (log roll 2))) 1))
    (list-ref l index)
    ]
   ['golden-ratio
    (define base 1.618033) ; phi to some precision
    (define n (length l))
    (define max_roll (exact-floor (expt base n)))
    (define roll (d 1 max_roll))
    (define index (- (- n (exact-floor (log roll base))) 1))
    (list-ref l index)]
   [else (error (format "Unknown distribution ~a" distribution))])
  )

(define (append-element lst elem)
  (append lst (list elem)))

(define (reduce lst func)
  (when (null? lst) (error "reduce: lst cannot be '()"))
  (if (null? (cdr lst))
      (car lst)
      (func (car lst) (reduce (cdr lst) func))))

(define (prune lst)
  (filter
   (λ (x) (and (not (null? x))
               (not (void? x))))
   lst))

(define (condense lst)
  (filter
   (λ (x) (and (not (null? x))
               (not (void? x))))
   (flatten lst)))

(define (insert-at lst pos x)
  (define-values (before after) (split-at lst pos))
  (append before (cons x after)))

(define (collect-similar lst)
  (hash->list
   (foldl (lambda (key ht)
            (hash-update ht key add1 0))
          '#hash()
          lst)))