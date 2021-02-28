#lang racket


(define player-actor%
  (class* object% ()
    (super-new)

    (define/public (get-next-command world) '())))

(define *actors* (list (new player-actor%)))

(provide (all-defined-out))