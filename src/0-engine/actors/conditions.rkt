#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "0-types/condition.rkt"

         "../core/io.rkt"
         "../core/utils.rkt"
         )

(lazy-require
 ["actor.rkt"
  (take-damage
   actor-conditions
   )]
 ["../combat/combat.rkt"
  (display-combatant-info
   )]
 ["../state/mutators.rkt"
  (pc
   )]
 ["../world/world.rkt"
  (get-actor)]
 ["../resolvers/round-resolver/simulation.rkt"
  (advance-time-by-iotas!)])

(define (condition-on-end-round! condition owner-id)
  (define owner (get-actor owner-id))
  (case (condition-type condition)
    ['bleeding
     (define bleed-damage-roll (d 1 6)) ; could give bonus from constitution here? say, 1d6?
     (cond ((= 1 bleed-damage-roll)
            (notice "Bleed check: 1d6 = 1: [1] => 1 dmg")
            (take-damage owner 1 'bleed)
            (display-combatant-info owner)
            )
           (else
            (notice (format "Bleed check: 1d6 = 1: [~a]" bleed-damage-roll))))]
    [else
     (dev-note (format "TODO: Something else: ~a" (condition-type condition)))])
  '())


; Wounds: Move from state to state
; Fresh wound [acquired-at]
; -> Treated wound [treated-at time-until-healed] (well treated / poorly treated; healing / not healing)
; -> Healed wound
; -> Poorly healed wound -> possible permanent condition = negative modifiers

(define (treat-wound! c)
  (define time-to-treat 10)
  (advance-time-by-iotas! time-to-treat)
  (notice (format "~a treated [~a ι]" (condition-type c) time-to-treat))
  )

(define (treat-wounds!)
  (for ([c (actor-conditions (pc))])
    (case (condition-type c)
      ['ankle-broken
      (p "Otava splints her purple, swollen ankle. She tries putting a little weight on it and immediately regrets it. There are multiple fractures in the small bones in her ankle.")
      (treat-wound! c)]
      ['both-ankles-broken
      (p "Otava splints her macerated ankles. She won't be walking anytime soon.")
      (treat-wound! c)]
      ['bleeding
      (p "Otava bandages her wounds.")
      (treat-wound! c)])
    ))