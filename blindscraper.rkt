#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "action.rkt")
(require "actor.rkt")
(require "utils.rkt")

(lazy-require ["martaanvuo.rkt" (engine-function pc)])

(define (make-blindscraper-action actor action-flag)
  (case action-flag

    ['attack
     (define damage-roll (λ () (d 1 2)))
     (define details
       (list
        (cons 'damage-roll damage-roll)
        (cons 'damage-roll-formula "1d2")
        ))
     (make-action
      #:symbol 'melee
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details details)]

    ['go-to-engaged
     (make-action
      #:symbol 'go-to-engaged
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details '())]

    ['blindscrape
     (make-action
      #:symbol 'inflict-status
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details '(blind))]

    [else
     (error (string-append
             "make-blindscraper-action: unknown action: "
             (symbol->string action-flag)))]))