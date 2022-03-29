#lang at-exp racket

(provide
 (all-defined-out))

(require racket/lazy-require)


(require
  "stance.rkt"

  "../2-core/io.rkt"
  "../2-core/core.rkt"

  "../3-types/action.rkt"
  "../3-types/status.rkt"
  "../3-types/actor.rkt"
  "../3-types/pc-actor.rkt"
  "../3-types/item.rkt"

  "../4-rules/actors/actor.rkt"
  "../4-rules/checks/checks.rkt"
  "../4-rules/world/world.rkt"
  "../4-rules/items/item.rkt"
  "../4-rules/pc/pc.rkt"

  "../7-state/state/state.rkt"
  )

(lazy-require ["combat.rkt"
  (get-combatant-name
   display-combatant-info
   display-pc-combatant-info
   add-combat-flag
   add-combat-event
   )])

(lazy-require ["../4-rules/locations/locations.rkt"
  (describe-begin-traverse-action
   describe-finish-traverse-action
   describe-cancel-traverse-action
   location-on-enter!
   )])

(lazy-require ["../4-rules/world/world.rkt"
  (get-actor
   )])

(lazy-require ["../5-resolvers/round-resolver/event-handler.rkt"
  (handle-interrupting-event!
   )])


; "generic" attack with type
(define (resolve-melee-action! action)
  (let/ec return
    (define actor (get-actor (action-actor-id action)))
    (when (not actor) (return 'actor-removed))
    (define target (get-actor (action-target action)))
    (when (not target) (return 'target-removed))
    (define target-defense (get-trait target "defense"))
    (define skill (get-trait actor "melee-attack-skill"))

    ; (define bonus 0)
    ; (cond ((member 'fallen (actor-statuses target))
    ;        (displayln "[Target fallen, TN -2]")
    ;        (set! bonus 2)
    ;        ))
    ; (cond ((engaged?)
    ;        (displayln "[Engaged, TN +1]")
    ;        (set! bonus -1)
    ;        ))
    ; (define action-target-number (- 7 bonus))

    (define title
      (format "Melee attack, ~a vs ~a"
              (get-combatant-name actor)
              (get-combatant-name target)))
    ; #;(define success? (skill-check title skill action-target-number))
    (define success? #t)

    (define damage-roll (melee-attack-action-damage-roll action))
    (define damage-roll-result (d
                                (standard-damage-roll-n damage-roll)
                                (standard-damage-roll-x damage-roll)
                                ; (standard-damage-roll-bonus damage-roll)
                                ))
    (define body
      (tbody
       (tr "damage"
           (format "~a: [~a]"
                   (damage-roll-formula damage-roll)
                   damage-roll-result))))
    (info-card body title)

    (define action-result 'ok)
    (when success? (set! action-result (take-damage target damage-roll-result 'melee)))
    (when (eq? action-result 'dead)
      ; move this to Actor
      (case (actor-name target)
        [("Blindscraper") (award-xp! 7)]))

    (display-combatant-info target)
    (newline)

    ; Urgh, refactor!
    (when (eq? action-result 'dead)
      (if (not (pc-actor? target))
          (p "The " (actor-name target) " is dead.")
          (begin
            (p "Otava is dead.")
            (set! action-result 'pc-dead))))


    (define descr
      (format "melee attack, ~a vs ~a (~a)"
              (get-combatant-name actor)
              (get-combatant-name target)
              (case action-result
                ['ok "hit"]
                ['dead "hit and kill"]
                [else action-result])))
    (add-combat-event descr)

    (return action-result)
    )
  )

(define (resolve-successful-shoot-action! action)
  (define actor (get-actor (action-actor-id action)))
  (define target (get-actor (action-target action)))
  (define title
    (format "Ranged [firearms], ~a vs ~a"
            (get-combatant-name actor)
            (get-combatant-name target)))

  (define success? #t)
  (consume-ammo! 1)

  (define details (action-details action))

  (define damage-roll (assoc 'damage-roll details))
  (define damage-roll-formula (cdr (assoc 'damage-roll-formula details)))
  (define damage-roll-result ((cdr damage-roll)))

  (when success?
    [notice (format "dmg: [~a] = [~a]" damage-roll-formula damage-roll-result)]
    (p "Otava pulls the trigger. The gun belts out a thunderous roar, and blood gushes out of the creature."))

  (define action-result 'ok)
  (when success? (set! action-result (take-damage target damage-roll-result 'melee)))
  (when (eq? action-result 'dead)

    (case (actor-name (action-target action))
      [("Blindscraper") (award-xp! 3)]))

  (display-combatant-info target)
  (newline)

  ; Urgh, refactor!
  (when (eq? action-result 'dead)
    (cond ((not (pc-actor? (action-target action)))
           (define text
             (take-random
              (list
               (format "The ~a collapses on the ground." (actor-name (action-target action)))
               (format "The ~a crumbles in a heap and twitches a few times, then goes still." (actor-name (action-target action))))))
           (p text))
          (else
           (begin
             #;(p "Otava is dead.")
             (set! action-result 'pc-dead)))))

  action-result
  )

(define (resolve-shoot-action! action)
  (define actor (get-actor (action-actor-id action)))
  (define target (get-actor (action-target action)))
  (define gun (get-firearm actor))
  (case (ranged-weapon-ammo-left gun)
    [(0) (p "Click. Out of ammo.")
     (award-xp! 1 "Whoops.")
     (set-flag 'aware-of-being-out-of-ammo)
     (increment-achievement! 'forgetful)
     'failure]
    [else (resolve-successful-shoot-action! action)]
    )
  )

(define (resolve-break-free-action! actor target [str-mod 0])
  (when (or (number? target)
            (symbol? target))
    (set! target (get-actor target)))
  (define target-stance (actor-stance target))

  (define statuses (actor-statuses actor))

  (when (not statuses)
    (set! statuses '()))
  (define actor-bound-status
    (findf (λ (status) (eq? (status-type status) 'bound))
           statuses))

  (cond (actor-bound-status
         (define target-number (status-lifetime actor-bound-status))

         (define dice-sides 4)
         (define bonus str-mod)
         (define roll (d 1 dice-sides))
         (define result (+ roll bonus))

         (define success?
           (cond ((= roll 1) #f)
                 ((= roll dice-sides) #t)
                 (else (> result target-number))))

         (define success-string
           (if success?
               "success"
               "failure"))

         (notice
          (format "Resolution: 1d10 + bonus > TN: ~a + ~a = ~a > ~a - ~a"
                  (number->string roll)
                  (number->string bonus)
                  (number->string result)
                  (number->string target-number)
                  success-string))
         ; crit = nat MAX = always succeed,
         ; crit fail = nat 1 = always fail, avoid hard failures?
         (wait-for-confirm)
         (cond (success?
                (p "Otava pulls her ankle free and stumbles back, just far enough to be out of reach of the writhing, searching hands.")
                (award-xp! 4)
                (define enemies (get-current-enemies))
                (define grabberkin (findf (λ (enemy) (eq? (actor-type enemy) 'grabberkin)) enemies))
                (remove-actor-from-its-current-location! grabberkin)
                (actor-remove-status! actor actor-bound-status)
                'ok)
               (else
                (p "The grip is still too strong for Otava to break it.")
                (award-xp! 1)
                'failed))
         )
        (else ; pc not bound
         (p "Grabberkin hand lets loose. Otava pulls her hurt foot free and stumbles back. When Otava turns to look, rotting grabberkin has disappeared, slithered back unto the mucid dark whence it came.")
         (award-xp! 3)
         ))
  )
