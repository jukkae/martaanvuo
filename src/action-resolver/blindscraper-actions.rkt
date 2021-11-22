#lang racket

(provide (all-defined-out))


(require racket/lazy-require)

(require
  "../actions/action.rkt"

  "../actors/actor.rkt"
  "../actors/pc-actor.rkt"
  "../actors/stance.rkt"

  "../core/checks.rkt"
  "../core/io.rkt"

  "../state/state.rkt"
  )


(lazy-require
 ["../state/combat.rkt"
  (get-combatant-name
   display-combatant-info
   display-pc-combatant-info
   add-combat-flag
   )])

(lazy-require
 ["../locations/locations.rkt"
  (describe-begin-traverse-action
   describe-finish-traverse-action
   describe-cancel-traverse-action
   location-on-enter!
   )])

(lazy-require
 ["../round-resolver/event-handler.rkt"
  (handle-interrupting-event!
   )])


; ability-like attack
(define (resolve-go-to-engaged-action! action)
  (define lp (pc-actor-lp (pc)))
  (define dex (actor-dexterity (action-actor action)))
  (define success?
    (cond ((positive? lp)
           (displayln "[LP positive]")
           (attribute-check "Dexterity" dex))
          (else #t)))
           
  (if success?
      (begin
        (p "The Blindscraper suddenly leaps forward and gets a hold of Otava's forearm with a couple of its lanky fingers. One of its long claws is swinging free, looking for an opening.")
                 
        (let ([enemy-stance (stance "α" 'engaged "right")])
          (set-actor-stance! (action-actor action) enemy-stance)))
        
      (begin
        (p "The Blindscraper leaps at Otava, but she dives under it and stumbles back to her feet.")
        (displayln "[-1 LP]")
        (set-pc-actor-lp! (pc)
                          (- (pc-actor-lp (pc))
                             1))
        (when (< (pc-actor-lp (pc)) 0)
          (set-pc-actor-lp! (pc)
                            0))
        (displayln (pc-actor-lp (pc)))
        'failure))
  'ok
  )

(define (resolve-go-to-close-action! action)
  (define lp (pc-actor-lp (pc)))
  (define dex (actor-dexterity (action-actor action)))
           
  (p "The Blindscraper skitters towards Otava.")
  
  (let ([enemy-stance (stance "α" 'close "right")])
    (set-actor-stance! (action-actor action) enemy-stance))
  'ok
  )