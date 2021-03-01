#lang racket

(require "actions.rkt")
(require "items.rkt")
(require "utils.rkt")
; see also lazy-require and units

(define pc%
  (class* object% ()
    (field [hp 4])
    (field [attack-skill 1])
    (field [inventory '(new knife%)])
    ;(field [inventory '()])
    (super-new)

    (define/public (get-brawl-damage) (d 1 2))
    (define/public (get-next-action) (error "not implemented yet!"))
    (define/public (hit dmg)
      (begin (set! hp (- hp dmg))
             (if (<= hp 0)
                 (begin (set! hp 0)
                        'u-ded)
                 'u-hit)))
    (define/public (get-status) (error "not implemented yet!"))
    (define/public (get-a-hunch)
      (take-random '(
                     "You feel something is watching you.")))))

(provide (all-defined-out))