#lang racket

(provide (all-defined-out))

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

(define (append-element lst elem)
  (append lst (list elem)))

(define (reduce lst func)
  (when (null? lst) (error "reduce: lst cannot be '()"))
  (if (null? (cdr lst))
      (car lst)
      (func (car lst) (reduce (cdr lst) func ))))

(define (all-fulfill-predicate? lst predicate)
  (define result #t)
  (for ([element lst])
    (when (not (predicate element))
      (set! result #f)))
  result)

(define-syntax-rule
  (proc body ...)
  (λ () body ...))
(define-syntax-rule
  (nop)
  (λ () '()))

; various container stuff
(define (collect-similar lst)
  (hash->list
   (foldl (lambda (key ht)
            (hash-update ht key add1 0))
          '#hash()
          lst)))


; Martaanvuo specific things that are still looking for their place
(define (time-of-day-from-jiffies jiffies)
  (define jiffies-of-current-day (remainder jiffies 400))
  (define time-of-day
    (cond ((< jiffies-of-current-day 100) 'morning)
          ((< jiffies-of-current-day 200) 'afternoon)
          ((< jiffies-of-current-day 300) 'evening)
          ((< jiffies-of-current-day 400) 'night)))
  time-of-day
  )