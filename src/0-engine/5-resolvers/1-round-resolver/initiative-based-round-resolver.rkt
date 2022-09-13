#lang at-exp racket

(require
  racket/lazy-require

  "ai.rkt"
  "action-queue.rkt"

  "../2-action-resolver/action-resolver.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"

  "../../3-types/action.rkt"
  "../../3-types/actor.rkt"
  "../../3-types/status.rkt"

  "../../4-systems/actors/actor.rkt"
  "../../4-systems/actors/statuses.rkt"
  "../../4-systems/checks/checks.rkt"
  "../../4-systems/pc/pc.rkt"
  "../../4-systems/world/world.rkt"

  "../../6-combat/combat.rkt"
  "../../6-combat/combat-action-resolver.rkt"

  "../../7-state/state.rkt"
  )

(lazy-require ["../../../1-content/narration/combat-narration.rkt" (
  get-combatant-name
  )])


(provide resolve-turns!)
(define (resolve-turns!)
  (let/ec end-round-early
    (when (all-actions-of-type? action-queue 'escape)
      (award-xp! 1)
      (end-combat)
      (end-round-early))

    (for ([action action-queue])

      (define actor (get-actor (action-actor-id action)))
      (define actor-removed?
        (or (not actor)
            (empty? (actor-location-id actor))))

      (cond
        [(not actor-removed?)
         (cond
           [(or (equal? (action-symbol action) 'get-closer)
                (equal? (action-symbol action) 'get-further))
            (cond [(pc-action? action)
                   (define enemy-movement-actions (find-all-enemy-movement-actions))
                   (cond [(not (null? enemy-movement-actions))
                          (just-roll "2d6" #:title "Otava")
                          (for ([a enemy-movement-actions])
                            (just-roll "2d6" #:title (get-combatant-name (get-actor (action-actor-id a)))))
                          (define check-result
                            (check "2d6"
                                   #:title (format "[~a] Contested roll" (get-combatant-name actor))
                                    #:target-number 6))
                          (cond [(equal? 'critical-success check-result)
                                 (notice "Critical success, gain fast!")
                                 (actor-add-status! actor (status 'fast 1))])
                          (cond [(equal? 'critical-failure check-result)
                                 (notice "Critical failure, lose fast and gain fallen!")
                                 (actor-remove-status-of-type! actor 'fast)
                                 (actor-add-status! actor (status 'fallen 1))])
                          (cond [(equal? 'narrow-success check-result)
                                 (notice "Narrow success, gain slow!")
                                 (actor-add-status! actor (status 'slow 1))])
                          (cond [(successful? check-result)
                                 (notice "Enemy movements discarded.")
                                 (discard-actions! enemy-movement-actions)]
                                [else
                                 (notice "Otava's movement discarded.")
                                 (discard-action! action)
                                 ]
                                )
                          '()]
                         )
                   ]
                  [(non-pc-action? action)
                   ; list!
                   (define pc-movement-actions (find-pc-movement-actions))
                   (cond [(not (null? pc-movement-actions))
                          (define check-result
                            (check "2d6"
                                   #:title (format "[~a] Contested roll" (get-combatant-name actor))
                                    #:target-number 9))
                          (cond [(equal? 'critical-success check-result)
                                 (notice "Critical success, gain fast!")
                                 (actor-add-status! actor (status 'fast 1))])
                          (cond [(equal? 'critical-failure check-result)
                                 (notice "Critical failure, lose fast and gain fallen!")
                                 (actor-remove-status-of-type! actor 'fast)
                                 (actor-add-status! actor (status 'fallen 1))])
                          (cond [(equal? 'narrow-success check-result)
                                 (notice "Narrow success, gain slow!")
                                 (actor-add-status! actor (status 'slow 1))])
                          (cond [(successful? check-result)
                                 (notice "Otava's movement discarded.")
                                 (discard-actions! pc-movement-actions)]
                                [else
                                 (notice "Enemy's movement discarded.")
                                 (discard-action! action)
                                 ])
                          '()]
                         )
                   ])])

         (define turn-result 'ok)

         (define pre-action-reaction? (get-pre-action-reaction action))
         (when (not (null? pre-action-reaction?))
           (set! action pre-action-reaction?))

         (when (not (equal? (action-symbol action) 'discarded))
           (set! turn-result (resolve-turn! action)))

         (define post-action-reaction-from-target? (get-post-action-reaction action turn-result))
         (when (not (null? post-action-reaction-from-target?))
           ;(define action post-action-reaction-from-target?)
           (dev-note (format "-- post-action-reaction-from-target?: ~a" post-action-reaction-from-target?)))

         (when (empty? (get-current-enemies))
           (set! turn-result 'end-combat)
           )

         (when (not (pc-is-alive?))
           (set! turn-result 'pc-dead))

         (case turn-result
           ['pc-dead
            (end-combat)
            (end-round-early)]

           ['end-combat
            (end-combat)
            (end-round-early)]
           )]

        [else 'skipped])
      )))

; (: -> Action TurnResult)
(define (resolve-turn! action)
  (let/ec return
    (define actor (get-actor (action-actor-id action)))
    (when (not actor) (return 'actor-removed))
    (when (not (null? (action-target action)))
      (define target (get-actor (action-target action)))
      (when (not target) (return 'target-removed)))

    (define turn-result
      (cond ((melee-attack-action? action)
             (resolve-melee-action! action))
            (else (resolve-action! action))))

    (when (not (pc-is-alive?))
      (dev-note "PC dead!")
      return 'pc-dead)
    turn-result
    ))

(define (end-combat)
  (remove-all-enemies-and-end-combat!)
  (clear-action-queue!))
