#lang racket

(define (wait-for-input)
  (displayln "-- in-universe side")
  (displayln "WAITING FOR INPUT...")
  (newline)
  (define input (read-line))
  (displayln "handling input: ")
  (displayln input)
  '(command))

(define player-actor%
  (class* object% ()
    (super-new)

    (define/public (get-next-command world)
      (wait-for-input))))

(define *actors* (list (new player-actor%)))

(provide (all-defined-out))