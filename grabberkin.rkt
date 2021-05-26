#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "action.rkt")
(require "actor.rkt")
(require "utils.rkt")

(lazy-require
 ["martaanvuo.rkt"
  (engine-function
   pc
   paragraph
   in-combat?
   actor-in-range?
   set-in-combat?!
   move-actor-to-location!
   current-location
   stance
   *enemy-stances*)])



(define (make-grabberkin-action actor action-flag)
  (case action-flag
    ['pull-under
     (define damage-roll (λ () (d 1 2)))
     (define details
       (list
        (cons 'damage-roll damage-roll)
        (cons 'damage-roll-formula "1d2")
        ))
     (make-action
      #:symbol 'pull-under
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details details)]

    ['anklebreaker
     (define damage-roll (λ () (d 1 2)))
     (define details
       (list
        (cons 'damage-roll damage-roll)
        (cons 'damage-roll-formula "1d2")
        ))
     (make-action
      #:symbol 'anklebreaker
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details details)]
    
    ['tighten-grip
     (define damage-roll (λ () (d 1 2)))
     (define details
       (list
        (cons 'damage-roll damage-roll)
        (cons 'damage-roll-formula "1d2")
        ))
     (make-action
      #:symbol 'tighten-grip
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details details)]
    
    ['skip
     (make-action
      #:symbol 'skip
      #:actor actor
      #:duration 0
      #:target '()
      #:tags '(initiative-based-resolution)
      #:details '(silent))]

    ['grab
     (make-action
      #:symbol 'inflict-status
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details (cons 'bound 3))]

    ['release-grip
     (make-action
      #:symbol 'release-grip
      #:actor actor
      #:duration 0
      #:target '()
      #:tags '(initiative-based-resolution)
      #:details '(fast))]

    [else
     (error (string-append
             "make-grabberkin-action: unknown action: "
             (symbol->string action-flag)))]))