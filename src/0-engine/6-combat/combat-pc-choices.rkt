#lang at-exp racket

(require
  "combat.rkt"

  "../2-core/core.rkt"

  "../3-types/action.rkt"
  "../3-types/actor.rkt"
  "../3-types/choice.rkt"

  "../4-systems/actors/actor.rkt"
  "../4-systems/pc/pc.rkt"
  "../4-systems/world/world.rkt"

  "../3-types/stance.rkt"

  "../7-state/state.rkt"

  "../../1-content/narration/combat-narration.rkt"
  )

(provide get-combat-choices)
(define (get-combat-choices)
  (define targets (get-current-enemies))

  (define combat-choices '())

  (set! combat-choices
        (append combat-choices (get-perceive-choices)))

  (when (actor-has-item? (pc) 'bolt-cutters)
    (set! combat-choices
          (append combat-choices (get-melee-choices)))
    (when (includes-enemy-of-type (append (get-enemies-at-range 'engaged)
                                          (get-enemies-at-range 'close))
                                  'grabberkin)
      (define e (includes-enemy-of-type (append (get-enemies-at-range 'engaged)
                                                (get-enemies-at-range 'close))
                                        'grabberkin))
      (set! combat-choices
            (append combat-choices (get-harvest-choice e)))))

  (when (actor-has-item? (pc) 'revolver)
    (set! combat-choices
          (append combat-choices (get-ranged-choices))))

  (cond ((and (not (engaged?))
              (not (actor-has-status-of-type? (pc) 'bound)))
         (define escape-choice
           (make-choice
            'escape
            "Escape."
            (λ ()
              (make-action
               #:symbol 'escape
               #:actor (pc)
               #:duration 1
               #:tags '(initiative-based-resolution fast)
               #:resolution-rules
               `(
                  (p "Otava escapes from combat.")
                  (wait-for-confirm)
                  'end-combat
                 )
                 ))))
         (set! combat-choices (append-element combat-choices escape-choice))))

  (define close-enemies (get-enemies-at-range 'close))
  (define close-grabberkin
    (filter (λ (enemy) (equal? (actor-type enemy)
                               'grabberkin))
            close-enemies))

  (cond ((and (not (null? close-grabberkin))
              (actor-has-status-of-type? (pc) 'bound))

         (define strength-mod (get-attribute-modifier-for (actor-strength (pc))))

         (define details
           (list 'str-mod strength-mod))

         (define target-id
           (actor-id (take-random close-grabberkin)))

         (define break-free-choice
           (make-choice
            'pull-free
            "Pull the leg free."
            (λ ()
              (make-action
               #:symbol 'break-free
               #:actor (pc)
               #:duration 1
               #:target target-id
               #:tags '(initiative-based-resolution fast)
               #:details details
               #:resolution-rules
               `((resolve-break-free-action! (pc) ,target-id ,strength-mod))))))
         (set! combat-choices (append-element combat-choices break-free-choice))))

  combat-choices
  )

; TODO: named attack definitions should be moved to content!
(define (make-named-attack action-name target)
  (define details
    `(list
      (cons 'damage-roll ,'(λ () (d 1 2)))
      (cons 'damage-roll-formula "1d2")
      (cons 'damage-type 'bludgeoning)
      ))
  (define attack-name
    (case action-name
      ['strike "Strike"]
      ['smash "Smash"]
      ['bludgeon "Bludgeon"]
      [else (symbol->string attack-name)]))
  (case action-name
    ['strike
     (define to-hit-bonus 0)
     (define to-hit-bonus-causes-text "Δ to hit: ")
     (cond
      [(pc-has-sense-organ? 'eyes)
       (define light-level (get-current-light-level))
       (case light-level
        ['bright
          (set! to-hit-bonus (+ to-hit-bonus 7))
          (set! to-hit-bonus-causes-text (string-append to-hit-bonus-causes-text (format "[eyes: ~a (bright light)]" to-hit-bonus)))]
        ['dark
          (set! to-hit-bonus (+ to-hit-bonus 3))
          (set! to-hit-bonus-causes-text (string-append to-hit-bonus-causes-text "[eyes: +3 (dark)]"))]
        ['pitch-black '()])]
      [(pc-has-sense-organ? 'sonar)
       (define bonus 0)
       (case (stance-range (actor-stance target))
          ['engaged
            (set! bonus 6)]
          ['close
            (set! bonus 7)]
          ['mid
            (set! bonus 3)]
          ['far
            (notice (format "The ~a is too far away for sonar." (get-combatant-name target)))
            (set! bonus 0)]
          [else (error)])
       (set! to-hit-bonus (+ to-hit-bonus 4))
       (when (> bonus 0)
        (set! to-hit-bonus-causes-text (string-append to-hit-bonus-causes-text (format "[sonar: ~a]" to-hit-bonus)))
        )
       ]
      )
     (define bonus-text
      (cond [(not (negative? to-hit-bonus))
             (format "+~a" to-hit-bonus)]
            [else
             (format "~a" to-hit-bonus)]
            ))
     (define damage-bonus 0)
     (define damage-bonus-text "+0")
     (make-choice
      'attack
      (format "~a the ~a~a. [~a, Δ dmg: ~a]"
              attack-name
              (get-combatant-name target)
              " [with bolt cutters]"
              to-hit-bonus-causes-text
              damage-bonus-text)
      (λ ()
        (define target-id (actor-id target))
        (make-action
         #:actor (pc)
         #:symbol 'strike
         #:duration 1
         #:target target-id
         #:resolution-rules
         `(
           (define actor (pc))
           (define target (get-actor ,target-id))
           (define target-defense (get-trait target "defense"))
           (define skill (get-trait actor "melee-attack-skill"))


           (define title
             (format "Melee attack, ~a vs ~a"
                     (get-combatant-name actor)
                     (get-combatant-name target)))

           (define target-number 10)
           (define to-hit-roll-result (+ (d 2 6) ,to-hit-bonus))
           (define success? (>= to-hit-roll-result target-number))
           (define success-text
            (if success?
              "success"
              "fail"))

           (define damage-roll-result (+ (d 1 2) ,damage-bonus))
           (define body
             (prune (tbody
              (tr "to hit"
                  (format "~ad~a ~a: [~a >= ~a] [~a]"
                          2 6 ,bonus-text
                          to-hit-roll-result
                          target-number
                          success-text))
              (tr "damage"
                  (format "~ad~a ~a: [~a]"
                          1 2 ,damage-bonus-text
                          damage-roll-result)))))
           (info-card body title)

           (define action-result 'ok) ; TODO: likely not useful anymore
           (when success? (set! action-result (take-damage target damage-roll-result 'melee)))
           (when (eq? action-result 'dead)
             ; TODO: move this to Actor
             (case (actor-name target)
               [("Blindscraper") (award-xp! 7)]))

           (display-combatant-info target)
           (newline)

           (when (eq? action-result 'dead)
             (p "The " (actor-name target) " is dead."))

           (define descr
             (format "melee attack, ~a vs ~a (~a)"
                     (get-combatant-name actor)
                     (get-combatant-name target)
                     (case action-result
                       ['ok "hit"]
                       ['dead "hit and kill"]
                       [else action-result])))
           (add-combat-event descr)

           action-result
           )
         )))]
    [else
     (make-choice
      'attack
      (format "~a the ~a~a."
              attack-name
              (get-combatant-name target)
              " [with bolt cutters]") ; TODO: weapon name
      (λ ()
        (define target-id (actor-id target))
        (make-melee-attack-action
         #:actor (pc)
         #:duration 1
         #:target target-id
         #:n 1
         #:x 2
         #:bonus 0
         )))]))

(define (get-melee-choices)
  (define targets (get-current-enemies))
  (define combat-choices '())
  (for ([i (in-range 0 (length targets))])
    (define target (list-ref targets i))
    (define stance (actor-stance target))
    (cond ((or (eq? (stance-range stance) 'close)
               (eq? (stance-range stance) 'engaged))
           (for ([action-name (list #;'smash #;'bludgeon 'strike)])
            (define choice (make-named-attack action-name target))
            (set! combat-choices (append-element combat-choices choice))
            ))
          ))
  combat-choices)

(define (get-perceive-choices)
  ; (define targets (get-current-enemies))
  ; (define perceive-choices '())
  ; (define choice
  ;   (make-choice 'perceive
  ;                 "Perceive. [with sonar]"
  ;                 (λ () (make-action #:symbol 'perceive-with-sonar
  ;                                    #:actor (pc)
  ;                                    #:duration 1
  ;                                    #:target null
  ;                                    #:tags '()
  ;                                    #:resolution-rules
  ;                                    '(
  ;                                     (for ([enemy (get-current-enemies)])
  ;                                       (define size (actor-size enemy))
  ;                                       (notice (format "There is a ~a something there." size)))
  ;                                     '()
  ;                                    )
  ;                                    ))))
  ; (set! perceive-choices (append-element perceive-choices choice))
  ; perceive-choices
  '()
  )

(define (get-harvest-choice target)
  (list
   (make-choice
    'harvest-finger
    (format "Harvest a finger from ~a [with bolt cutters]." (get-combatant-name target))
    (λ ()
      (define target-id (actor-id target))
      (make-action
       #:symbol 'harvest-finger
       #:actor (pc)
       #:duration 1
       #:target target-id
       #:tags '(initiative-based-resolution fast)
       #:resolution-rules
       `(
         (add-item! 'grabberkin-finger)
         (wait-for-confirm)
         ))
      ))))



(define (get-ranged-choices)
  (define targets (get-current-enemies))

  (define all-choices
    (for/list ([i (in-range 0 (length targets))])
      (define target (list-ref targets i))
      (define target-id (actor-id target))
      (define stance (actor-stance target))

      (list
       (when (and (not (flag-set? 'aware-of-being-out-of-ammo))
                  (or (eq? (stance-range stance) 'far) ; always require roll
                      (eq? (stance-range stance) 'mid) ; require roll if no proficiency
                      (eq? (stance-range stance) 'close) ; never require roll
                      (eq? (stance-range stance) 'engaged)))
         (define damage-roll (λ () (d 2 2)))
         (define details
           (list
            (cons 'damage-roll damage-roll)
            (cons 'damage-roll-formula "2d2")
            (cons 'damage-type 'gunshot) ; we're assuming firearms here
            ))

         (make-choice
          'attack
          (format "Shoot ~a [with revolver]." (get-combatant-name target))
          (λ ()
            (make-action
             #:symbol 'shoot
             #:actor (pc)
             #:duration 1
             #:target target-id
             #:tags '(initiative-based-resolution)
             #:details details
             #:resolution-rules
             '(resolve-as-shoot-action)))))

       (when (or (eq? (stance-range stance) 'close)
                 (eq? (stance-range stance) 'engaged))

         (make-choice
          'attack
          (format "Pistol whip the ~a [with revolver]." (get-combatant-name target))
          (λ ()
            (define target-id (actor-id target))
            (make-melee-attack-action
             #:actor (pc)
             #:duration 1
             #:target target-id
             #:n 1
             #:x 1
             #:bonus 0
             #:damage-type 'bludgeoning
             ))))
       )))

  (condense all-choices))
