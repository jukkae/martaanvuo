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

  (define combat-choices
    (list
     (make-choice
      'skip
      "Skip. [gain 'fast for next round]" ; TODO: implement this
      (λ ()
        (make-action
         #:symbol 'skip
         #:actor (pc)
         #:duration 0 ; TODO: a turn automatically consumes 1 tick even when this is 0
         #:target '()
         #:tags '(initiative-based-resolution)
         #:details '(slow silent)
         #:resolution-rules `('())
         )))
     ))

  (set! combat-choices
        (append combat-choices (get-perceive-choices)))
  (when (actor-has-item? (pc) 'bolt-cutters)
    (set! combat-choices
          (append combat-choices (get-melee-choices)))
    (when (includes-enemy-of-type (append (get-enemies-at-range 'engaged)
                                          (get-enemies-at-range 'adjacent)
                                          (get-enemies-at-range 'close))
                                  'grabberkin)
      (define e (includes-enemy-of-type (append (get-enemies-at-range 'engaged)
                                                (get-enemies-at-range 'adjacent)
                                                (get-enemies-at-range 'close))
                                        'grabberkin))
      (set! combat-choices
            (append combat-choices (get-harvest-choice e)))))


  (when (actor-has-item? (pc) 'revolver)
    (set! combat-choices
          (append combat-choices (get-ranged-choices))))
  (cond ((and (not (engaged?))
              (empty? (get-enemies-at-range 'adjacent))
              (empty? (get-enemies-at-range 'close))
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
                  (notice "Otava escapes from combat.")
                  'end-combat
                 )
                 ))))
         (set! combat-choices (append-element combat-choices escape-choice))))

  (cond [(and (not (engaged?))
              (not (actor-has-status-of-type? (pc) 'bound)))

         (define engaged-enemies (get-enemies-at-range 'engaged))
         (define adjacent-enemies (get-enemies-at-range 'adjacent))
         (define close-enemies (get-enemies-at-range 'close))
         (define nearby-enemies (get-enemies-at-range 'nearby))
         (define far-enemies (get-enemies-at-range 'far))
         (cond
           [(and (empty? adjacent-enemies)
                 (or (not (empty? close-enemies))
                     (not (empty? nearby-enemies))
                     (not (empty? far-enemies))))
            (define enemies-in-range (append* close-enemies nearby-enemies far-enemies))
            (for ([target enemies-in-range])
              (define target-stance-range
                (case (stance-range (actor-stance target))
                  ['close 'adjacent]
                  ['nearby 'close]
                  ['far 'nearby]))
              (define target-id (actor-id target))
              (define get-closer-choice
                (make-choice
                 'get-closer
                 (format "Get closer to ~a [target range: ~a]." (get-combatant-name target) target-stance-range)
                 (λ ()
                   (make-action
                    #:symbol 'get-closer
                    #:actor (pc)
                    #:duration 1
                    #:tags '(initiative-based-resolution fast)
                    #:resolution-rules
                    `(
                      (set-actor-stance-range! (get-actor ,target-id) ',target-stance-range #t)
                      )
                    ))))
              (set! combat-choices (append-element combat-choices get-closer-choice))
              )

            ])
         (cond
           [(or (not (empty? adjacent-enemies))
                (not (empty? close-enemies))
                (not (empty? nearby-enemies)))
            (define get-further-choice
              (make-choice
               'get-further
               "Get away."
               (λ ()
                 (make-action
                  #:symbol 'get-further
                  #:actor (pc)
                  #:duration 1
                  #:tags '(initiative-based-resolution fast)
                  #:resolution-rules
                  `(
                    (for ([enemy (get-current-enemies)])
                      (case (stance-range (actor-stance enemy))
                        ['adjacent
                         (set-actor-stance-range! enemy 'close #t)
                         ]
                        ['close
                         (set-actor-stance-range! enemy 'nearby #t)
                         ]
                        ['nearby
                         (cond
                          [(pc-has-sense-organ? 'eyes) ; TODO: or if pc has hearing and enemy is not silent
                           (set-actor-stance-range! enemy 'far #t)
                           ]
                          [else
                           (define roll (d 2 6))
                           (notice (format "Random walk roll (2d6 >= 7): [~a]" roll))
                           (cond
                            [(>= roll 7)
                             (set-actor-stance-range! enemy 'far #t)]
                            [else
                             (notice "Otava stumbles in the wrong direction.")
                             ]
                           )
                           ]
                          )
                         ]))
                    )
                  ))))
            (set! combat-choices (append-element combat-choices get-further-choice))
            ])

         ])

  (define close-grabberkin
    (filter (λ (enemy) (equal? (actor-type enemy)
                               'grabberkin))
            (get-enemies-at-range 'close)))

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

; returns (list bonus descr)
(define (get-to-hit-bonus target)
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
        (set! bonus 5)]
       ['adjacent
        (set! bonus 7)]
       ['close
        (set! bonus 6)]
       ['nearby
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
  (list to-hit-bonus to-hit-bonus-causes-text))

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
      ['choke "Choke"]
      [else (symbol->string attack-name)]))
  (case action-name
    ['strike
     (define b (get-to-hit-bonus target))
     (define to-hit-bonus (first b))
     (define to-hit-bonus-causes-text (second b))
     (define bonus-text
      (cond [(not (negative? to-hit-bonus))
             (format "+~a" to-hit-bonus)]
            [else
             (format "~a" to-hit-bonus)]
            ))
     (define damage-bonus 0)
     (define damage-bonus-text "+0")
     (when (eq? (stance-range (actor-stance target)) 'engaged)
      (set! damage-bonus -1)
      (set! damage-bonus-text "-1 (engaged)"))
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
         #:tags '(initiative-based-resolution)
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
                     (when success?
                       (tr "damage"
                           (format "~ad~a ~a: [~a]"
                                   1 2 ,damage-bonus-text
                                   damage-roll-result))))))
           (info-card body title)

           (define action-result 'ok) ; TODO: likely not useful anymore
           (when success? (set! action-result (take-damage target damage-roll-result 'melee)))
           (when (and (eq? (stance-range (actor-stance target)) 'engaged)
                      success?
                      (eq? (actor-size target) 'small))
             (notice (format "The ~a is pushed back." (actor-name target))))
           (set-actor-stance-range! target 'adjacent #f) ; #f reads better here
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
    ['choke
     (make-choice
      'attack
      (format "Strangle the ~a."
              (get-combatant-name target)
              )
      (λ ()
        (define target-id (actor-id target))
        (make-action
         #:actor (pc)
         #:symbol 'choke
         #:duration 1
         #:target target-id
         #:tags '(initiative-based-resolution)
         #:resolution-rules
         `(
           (define actor (pc))
           (define target (get-actor ,target-id))

           (define title
             (format "Strangle, ~a vs ~a"
                     (get-combatant-name actor)
                     (get-combatant-name target)))
           (when (not (eq? (stance-range (actor-stance target)) 'engaged))
             (set-actor-stance-range! target 'engaged #t))
           (define attack-roll (d 1 4))
           (notice (format "[1d4: ~a]" attack-roll))
           (define action-result 'ok) ; TODO: likely not useful anymore
           (case attack-roll
             [(1 2) (notice (format "Otava can't get a good grip on the ~a's windpipe." (get-combatant-name target)))]
             [(3)
              (cond [(not (actor-has-condition-of-type? target 'windpipe-broken))
                     (notice (format "There's a crack as the ~a's windpipe breaks." (get-combatant-name target)))
                     (inflict-condition! target
                                         (condition 'windpipe-broken
                                                    "Broken windpipe."))
                     (set! action-result (take-damage target 1 'strangling))
                     ]
                    [else ; has 'windpipe-broken
                     (notice (format "Otava easily strangles the ~a to death." (get-combatant-name target)))
                     (kill target 'strangled)
                     (set! action-result 'dead)
                     ]
                    )
              ]
             [(4)
              (notice (format "Otava strangles the ~a to death." (get-combatant-name target)))
              (kill target 'strangled)
              (set! action-result 'dead)])

           (when (eq? action-result 'dead)
             ; TODO: move this to Actor
             (case (actor-name target)
               [("Voidfloater") (award-xp! 3)]))

           (display-combatant-info target)
           (newline)

           (when (eq? action-result 'dead)
             (notice "The " (actor-name target) " is dead."))

           (define descr
             (format "strangulation, ~a vs ~a (~a)"
                     (get-combatant-name actor)
                     (get-combatant-name target)
                     (case action-result
                       ['ok "successful"]
                       ['dead "strangled to death"]
                       [else action-result])))
           (add-combat-event descr)

           action-result
           )
         )))
     ]
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
    (cond ((or (eq? (stance-range stance) 'engaged) ; TODO: in-melee-range?
               (eq? (stance-range stance) 'adjacent)
               (eq? (stance-range stance) 'close))
           (for ([action-name (list #;'smash #;'bludgeon 'strike)])
             (define choice (make-named-attack action-name target))
             (set! combat-choices (append-element combat-choices choice))
             ))
          )
    (cond ((or (eq? (stance-range stance) 'engaged)
               (eq? (stance-range stance) 'adjacent))
           (for ([action-name (list 'choke)])
             (define choice (make-named-attack action-name target))
             (set! combat-choices (append-element combat-choices choice))
             )))
    )
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
                      (eq? (stance-range stance) 'nearby) ; require roll if no proficiency
                      (eq? (stance-range stance) 'close) ; never require roll
                      (eq? (stance-range stance) 'adjacent)
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

       (when (or (eq? (stance-range stance) 'engaged)
                 (eq? (stance-range stance) 'adjacent)
                 (eq? (stance-range stance) 'close))

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
