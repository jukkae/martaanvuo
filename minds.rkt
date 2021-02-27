#lang racket

(define (wait-for-input)
  (displayln "WAITING FOR INPUT...")
  (newline)
  (define input (read-line))
  (displayln "handling input: ")
  (displayln input))

(define player-mind%
  (class* object% ()
    (super-new)

    (define/public (get-next-action world)
      (wait-for-input))))

(define *minds* (list (new player-mind%)))

(provide (all-defined-out))