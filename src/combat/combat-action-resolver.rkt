#lang at-exp racket

(provide
 (all-defined-out))

(require racket/lazy-require)

(require rebellion/collection/association-list)

(lazy-require
 ["combat.rkt"
  (get-combatant-name
   display-combatant-info
   display-pc-combatant-info
   add-combat-flag
   add-combat-event
   )])

(require
  "stance.rkt"

  "../actions/action.rkt"
  "../actors/actor.rkt"
  "../actors/status.rkt"
  "../core/checks.rkt"
  "../core/io.rkt"
  "../items/item.rkt"
  "../pc/pc.rkt"
  "../actors/pc-actor.rkt"
  "../state/state.rkt"
  "../core/utils.rkt")


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


; "generic" attack with type
(define (resolve-melee-action! action)
  (define actor (action-actor action))
  (define target (action-target action))
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

  (define details (action-details action))

  (define damage-roll (assoc 'damage-roll details))
  (define damage-roll-formula (cdr (assoc 'damage-roll-formula details)))
  (define damage-roll-result ((cdr damage-roll)))

  (define body
    (tbody
      (tr " damage "
          (format " ~a: [~a] " damage-roll-formula damage-roll-result))))
  (info-card body title)

  (define action-result 'ok)
  (when success? (set! action-result (take-damage target damage-roll-result 'melee)))
  (when (eq? action-result 'dead)
    ; TODO what's a smart place to store this? the actor?
    (case (actor-name (action-target action))
      [("Blindscraper") (award-xp! 7)]))

  (display-combatant-info target)
  (newline)

  ; Urgh, refactor!
  (when (eq? action-result 'dead)
    (if (not (pc-actor? (action-target action)))
        (p "The " (actor-name (action-target action)) " is dead.")
        (begin
          (p "Otava is dead.")
          (set! action-result 'pc-dead))))

  (add-combat-event "melee hit!")

  action-result
  )

(define (resolve-successful-shoot-action! action)
  (define actor (action-actor action))
  (define target (action-target action))
  (define title
    (format "Ranged [firearms], ~a vs ~a"
            (get-combatant-name actor)
            (get-combatant-name target)))

  ; TODO add sophistication regarding ranges etc
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
  (define actor (action-actor action))
  (define target (action-target action))
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

(define (resolve-break-free-action! action)
  (define actor (action-actor action))
  (define details (action-details action))
  (define str-mod (vector-ref (association-list-ref details 'str-mod) 0))

  (define target (action-target action))
  (define target-stance (actor-stance target))

  (define statuses (actor-statuses actor))
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
           (dev-note "Fix me: shouldn't end combat")
           'end-combat) ; shouldn't be end-combat!
          (else
           (p "The grip is still too strong for Otava to break it.")
           (award-xp! 1)
           'failed))
  )
  (else ; pc not bound
    (p "Grabberkin hand lets loose. Otava pulls her hurt foot free and stumbles back. When Otava turns to look, rotting grabberkin has disappeared, slithered back unto the mucid dark whence it came.")
    (award-xp! 3)
    (dev-note "Fix me: shouldn't end combat")
    'end-combat
    ))
  )


; skinnable, but in a sense generic action
(define (resolve-flee-action! action)
  (cond ((pc-actor? (action-actor action))
         (p "Otava turns her back to run.")
         (define skill (get-trait (pc) "athletics-skill"))

         (define stance-range-values '())
         (for ([enemy (get-current-enemies)])
           (define stance (actor-stance enemy))
           (define value (get-stance-range-numeric-value (stance-range stance)))
           (set! stance-range-values (append-element stance-range-values value)))
         (define target-number
           ; if there's an enemy in engaged range, then more difficult check
           (if (member 0 stance-range-values)
               10
               8))

         (define success? (skill-check "Athletics" skill target-number))
         (if success?
             (begin ; TODO wouldn't it be cool if only failure was explicitly noted :D
               (p "She dives behind a small bush and waits.")
               (wait-for-confirm)
               (if (luck-check)
                   (p "PASS")
                   (p "FAIL"))
               (p "Nothing seems to be following her.")
               (award-xp! 3 "for a working survival instinct")
               'end-combat)
             (begin
               (p "Otava's foot gets caught on a root. She falls face down in the mud.")
               (actor-add-status! (pc) (status 'fallen 1))
               (display-pc-combatant-info (pc))
               (wait-for-confirm)
               'failure))
         )

        (else ; not a pc actor
         (p
          (format "~a tries to run." (get-combatant-name (action-actor action))))
         (define skill 1)
         (define stance (actor-stance (action-actor action)))
         (define value (get-stance-range-numeric-value (stance-range stance)))
         (define target-number
           (if (= value 0)
               10
               8))

         (define success? (skill-check "Athletics" skill target-number))
         (if success?
             ; TODO this fails if there are multiple enemies!
             (begin
               (p "The Blindscraper skitters away and disappears in the foliage.")
               (award-xp! 1)
               'escape-from-combat)
             (begin
               (p "It is fast, but not fast enough.")
               (actor-add-status! (action-actor action) (status 'fallen 1))
               (display-combatant-info (action-actor action))
               'failure))
         )))


(define (resolve-inflict-status-action! action)
  (define target (action-target action))
  (define status (car (action-details action)))
  (when (status? status)
    (when (eq? (status-type status) 'bound)
      (p "The Grabberkin seems to realize its grip is loosening. Its rotting fingers curl around Otava's ankle again with dreadful might.")))

  (inflict-status! target status)
  'ok)

(define (resolve-modify-status-action! action)
  (define target (action-target action))
  (define status (car (action-details action)))
  (when (eq? (status-type status) 'bound) ; this is shit, refactor
    (p "The Grabberkin seems to realize its grip is loosening. Its rotting fingers curl around Otava's ankle again with dreadful might.")
    (define amount (status-lifetime status))
    (modify-actor-status-lifetime target 'bound amount)
    )
  'ok)

(define (resolve-inflict-condition-action! action)
  (define target (action-target action))
  (define condition (car (action-details action)))
  (displayln "action-resolver: resolve-action!: inflict-condition: TODO")
  #;(when (eq? (status-type status) 'bound)
      (p "The Grabberkin seems to realize its grip is loosening. Its rotting fingers curl around Otava's ankle again with dreadful might."))
  #;(inflict-status! target status)
  'ok)
