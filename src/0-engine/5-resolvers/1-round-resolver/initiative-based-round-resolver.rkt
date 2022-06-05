#lang at-exp racket

(require
  "ai.rkt"
  "action-queue.rkt"

  "../2-action-resolver/action-resolver.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"

  "../../3-types/action.rkt"
  "../../3-types/actor.rkt"

  "../../4-systems/actors/actor.rkt"
  "../../4-systems/pc/pc.rkt"
  "../../4-systems/world/world.rkt"

  "../../6-combat/combat.rkt"
  "../../6-combat/combat-action-resolver.rkt"

  "../../7-state/state.rkt"
  )


(provide resolve-turns!)
(define (resolve-turns!)
  (let/ec end-round-early
    (when (all-actions-of-type? action-queue 'escape)
      (award-xp! 1)
      (end-combat)
      (end-round-early))
    (for ([action action-queue])

      (define actor (get-actor (action-actor-id action)))
      ; a bit hacky check to see if this actor has been removed already
      (define actor-removed?
        (or (not actor)
            (empty? (actor-location-id actor))))

      (cond ((not actor-removed?)
             (define pre-action-reaction? (get-pre-action-reaction action))
             (when (not (null? pre-action-reaction?))
               (set! action pre-action-reaction?))

             (define turn-result (resolve-turn! action))

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
               ))

            (else
             'skipped))

      )))

; (: -> Action TurnResult)
(define (resolve-turn! action)
  (cond ((melee-attack-action? action)
         (resolve-melee-action! action))
        (else (resolve-action! action))))

(define (end-combat)
  (remove-all-enemies-and-end-combat!)
  (clear-action-queue!))
