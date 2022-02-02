#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../actions/action.rkt"

  "../actors/actor.rkt"
  "../actors/condition.rkt"

  "../core/io.rkt"
  "../core/utils.rkt"

  "../state/state.rkt"
  )

(lazy-require
 ["../combat/combat.rkt"
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
(define (resolve-pull-under-action! action)
  (p "The hands grasping her ankle – the thing with the hands – shift in the waters under the floating raft of moss. The thing pulls Otava through the moss, through a thick layer of algae, into the cloudy waters. The heavy, dark water closes in around her.")
  (p "The thing pulls her deeper. She fights back, but her arms get caught in the massive algae congesting the grimy waters. She cannot hold her breath much longer.") ; -> fragments -> saving throw, not direct death
  (wait-for-confirm)

  (p "Otava opens her mouth and drowns four feet under the surface of a nameless pool in Martaanvuo.") ; -> todo: name it 'the drowning pools' in subsequent rounds
  (kill (pc) 'drowned)
  )

;;; CRIT ROLL IDEA:
; Base chances are like 1/6 for "certain" attack failing, 1/6 for crit,
; but PC's attribute/skill bonuses can shift that balance
; - or would failures be based on saving throw? maybe.

; move to utils
(define (roll-crit? sides)
  (define crit-roll (d 1 sides))
  (define critical? (= crit-roll 6))
  (define crit-string (if critical?
                          ", crit"
                          ""))
  (notice (format "crit roll: 1d~a = ~a~a" sides crit-roll crit-string)))

; ability-like attack
(define (resolve-anklebreaker-action! action)
  (define target (action-target action))
  (cond ((not (actor-has-condition-of-type? target 'ankle-broken)) ; first
         (p "The hands tighten their vice-like hold on Otava's ankle. There's a wet, crunchy sound as bones shatter and tear through the surrounding muscle.")
         (define critical? (roll-crit? 4))
         (when critical?
           (p "A shard of bone sticks out through a gash in her ankle. Blood starts to flow."))
         (define action-result (take-damage target 1 'trauma))
         (case action-result
           ('hit
            (inflict-condition!
             target
             (condition 'ankle-broken "resolve-anklebreaker-action!: details for 'ankle-broken todo"))
            (when critical?
              (inflict-condition!
               target

               (condition 'bleeding ; TODO: This kind of involved definition belongs to, say, conditions.rkt or something
                          ;"resolve-anklebreaker-action!: details for 'bleeding todo"
                          '() ; details
                          )))
            (display-combatant-info target)
            'ok)
           ('dead
            (display-combatant-info target)
            'pc-dead)
           (else (error (format "unhandled action-result ~a" action-result))))
         )

        ; second ankle
        (else
         (p "The Grabberkin shifts its hands onto Otava's other ankle with ease, as if it's slowly waking up, and crushes the bones in Otava's other ankle, too.")

         (define critical? (roll-crit? 4))
         (when critical?
           (p "A sharp edge of a broken bone punctures an artery and blood gushes out."))

         (define action-result (take-damage (action-target action) 1 'trauma))
         (display-combatant-info (action-target action))
         (case action-result
           ('hit
            (inflict-condition!
             (action-target action) (condition
                                     'ankle-broken
                                     "resolve-anklebreaker-action!: details todo"))
            (when critical?
              (inflict-condition!
               target

               (condition 'bleeding ; TODO: This kind of involved definition belongs to, say, conditions.rkt or something
                          ;"resolve-anklebreaker-action!: details for 'bleeding todo"
                          '() ; details
                          #;(λ ()
                              (define bleed-damage-roll (d 1 6)) ; could give bonus from constitution here? say, 1d6?
                              (cond ((= 1 bleed-damage-roll)
                                     (notice "Bleed check: 1d6 = 1: [1] => 1 dmg")
                                     (take-damage target 1 'bleed)
                                     (display-combatant-info target)
                                     )
                                    (else
                                     (notice (format "Bleed check: 1d6 = 1: [~a]" bleed-damage-roll)))))


                          )))
            'ok)
           ('dead
            'pc-dead)
           (else (error (format "resolve-anklebreaker-action!: unhandled action-result ~a" action-result)))))))
