#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../../2-core/io.rkt"
  "../../2-core/core.rkt"
  "../../3-types/condition.rkt"
  "../../3-types/actor.rkt"
  )

(lazy-require ["actor.rkt"
  (take-damage
   )])
(lazy-require ["../simulation.rkt"
  (advance-time-by-iotas!
   )])
(lazy-require ["../world/world.rkt"
  (get-actor
   )])
(lazy-require ["../../7-state/mutators.rkt"
  (pc
   )])
(lazy-require ["../../7-state/state.rkt"
  (current-elapsed-time
   )])
(lazy-require ["../../../1-content/narration/combat-narration.rkt"
  (display-combatant-info
   )])

(define (condition-on-end-round! condition owner-id)
  (define owner (get-actor owner-id))
  (case (condition-type condition)
    ['bleeding-profusely
     (define bleed-damage-roll (d 1 2))
     (notice (format "Profusely bleeding: [dmg: 1d2]: [~a]" bleed-damage-roll))
     (take-damage owner bleed-damage-roll 'bleed)
     ]
    ['bleeding
     (define bleed-damage-roll (d 1 6)) ; could give bonus from constitution here? say, 1d6?
     (cond ((= 1 bleed-damage-roll)
            (notice "Bleed check: 1d6 = 1: [1] => 1 dmg")
            (take-damage owner 1 'bleed)
            (display-combatant-info owner)
            )
           (else
            (notice (format "Bleed check: 1d6 = 1: [~a]" bleed-damage-roll))))]
    ['windpipe-broken
     (dev-note "windpipe crushed end-of-round")]
    [else
     '()
     #;(dev-note (format "TODO: Something else: ~a" (condition-type condition)))])
  '())

; Fresh injury [acquired-at]
; -> Treated injury [treated-at time-until-healed] (well treated / poorly treated; healing / not healing)
; -> Healed injury
; -> Poorly healed injury -> possible permanent condition = negative modifiers

(define (treat-injury! c)
  (define time-to-treat 10)
  (advance-time-by-iotas! time-to-treat)
  (notice (format "~a treated [~a ι]" (condition-type c) time-to-treat))
  )

(define (treat-injuries!)
  (for ([c (actor-conditions (pc))])
    (case (condition-type c)
      ['ankle-broken
      (p "Otava splints her purple, swollen ankle. She tries putting a little weight on it and immediately regrets it. There are multiple fractures in the small bones in her ankle.")
      (treat-injury! c)]
      ['both-ankles-broken
      (p "Otava splints her macerated ankles. She won't be walking anytime soon.")
      (treat-injury! c)]
      ['bleeding
      (p "Otava bandages her wounds.")
      (treat-injury! c)])
    ))



(define (actor-add-condition! actor condition)
  (when (not (null? actor))
    (notice (format "~a: Condition [~a] added, details: [~a]" (actor-name actor) (condition-type condition) (condition-details condition))))
  (set-actor-conditions! actor (append-element (actor-conditions actor) condition)))

(define (actor-remove-condition! actor condition)
  (when (not (null? actor))
    (notice (format "~a: Condition [~a] removed" (actor-name actor) (condition-type condition))))
  (set-actor-conditions! actor (remove condition (actor-conditions actor))))

(define (actor-remove-condition-of-type! actor type)
  (when (not (null? actor))
    (notice (format "~a: Condition of type [~a] removed" (actor-name actor) type)))
  (set-actor-conditions! actor (filter
                                (λ (other) (not (equal? type
                                                     (condition-type other))))
                                (actor-conditions actor))))

(define (actor-has-condition-of-type? actor type)
  (if (memf (λ (condition)
              (equal? (condition-type condition) type))
            (actor-conditions actor))
      #t
      #f))

(define (actor-get-condition-of-type actor type)
  (findf (λ (condition)
           (equal? (condition-type condition) type))
         (actor-conditions actor)))

(define (condition-age condition)
  (- (current-elapsed-time) (condition-acquired-at condition)))
