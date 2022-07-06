#lang at-exp racket

(require
  "combat.rkt"
  "narration.rkt"

  "../2-core/core.rkt"

  "../3-types/action.rkt"
  "../3-types/actor.rkt"
  "../3-types/choice.rkt"

  "../4-systems/actors/actor.rkt"

  "../6-combat/stance.rkt"

  "../7-state/state.rkt"
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
                 (define enemies (get-current-enemies))
                 #;(cond ((> (length enemies) 1)
                        (displayln "resolve-escape-action!: Narration for multiple enemies"))
                       ((= (length enemies) 0)
                        (displayln "resolve-escape-action!: Narration for zero enemies"))
                       (else
                        (define enemy (get-current-enemy))
                        (case (actor-type enemy)
                          ['blindscraper
                           (displayln "escaping from blindscraper")
                           ]
                          ['grabberkin
                           (displayln "escaping from grabberkin")
                           ])
                        )
                       )
                 (define skill (get-trait (pc) "athletics-skill"))
                ; TODO: blurbify escape narration based on location and enemies

                 (define stance-range-values '())
                 (for ([enemy enemies])
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
                     (begin
                       (p "Otava escapes from combat.")
                       (wait-for-confirm)
                       'end-combat
                       )
                     (begin
                       (p "Otava fails to escape from combat.")
                       (wait-for-confirm)
                       'failure))
                 )))))
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
  ;(define damage-roll '(λ () d 1 2))
  (define details
    `(list
      (cons 'damage-roll ,'(λ () (d 1 2)))
      (cons 'damage-roll-formula "1d2")
      (cons 'damage-type 'bludgeoning)
      ))
  (define attack-name
    (case action-name
      ['smash "Smash"]
      ['parry "Parry"]
      ['bludgeon "Bludgeon"]
      [else (symbol->string attack-name)]))
  (define choice
    (make-choice
    'attack
    (format "~a the ~a [with bolt cutters]."
            attack-name
            (get-combatant-name target))
    (λ ()
      (define target-id (actor-id target))
      (make-melee-attack-action
        #:actor (pc)
        #:duration 1
        #:target target-id
        #:n 1
        #:x 2
        #:bonus 0
        ))))
  choice)

(define (get-melee-choices)
  (define targets (get-current-enemies))
  (define combat-choices '())
  (for ([i (in-range 0 (length targets))])
    (define target (list-ref targets i))
    (define stance (actor-stance target))
    (cond ((or (eq? (stance-range stance) 'close)
               (eq? (stance-range stance) 'engaged))
           (for ([action-name (list 'smash 'parry 'bludgeon)])
            (define choice (make-named-attack action-name target))
            (set! combat-choices (append-element combat-choices choice))
            ))
          ))
  combat-choices)

(define (get-perceive-choices)
  (define targets (get-current-enemies))
  (define perceive-choices '())
  (define choice
    (make-choice 'perceive
                  "Perceive. [with eyes]"
                  (λ () (make-action #:symbol 'perceive-with-eyes
                                     #:actor (pc)
                                     #:duration 1
                                     #:target null
                                     #:tags '()
                                     ))))
  (set! perceive-choices (append-element perceive-choices choice))
  perceive-choices)

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
