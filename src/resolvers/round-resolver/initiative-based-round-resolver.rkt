#lang at-exp racket

(require racket/lazy-require)

(require
  "ai.rkt"
  "action-queue.rkt"

  "../action-resolver/action-resolver.rkt"

  "../../actors/0-types/actor.rkt"

  "../../actions/action.rkt"

  "../../combat/combat.rkt"

  "../../world/world.rkt"

  "../../core/io.rkt"
  "../../core/utils.rkt"

  "../../pc/pc.rkt"

  "../../state/state.rkt")

(lazy-require ["../../world.world.rkt" (get-actor)])


(provide resolve-turns!)
(define (resolve-turns!)
  (let/ec end-round-early
    (when (all-actions-of-type? action-queue 'flee)
      (p "Otava turns and ducks in the shadows. She waits a while.")
      (award-xp! 1)
      (end-combat)
      (end-round-early))
    (for ([action action-queue])

      (define actor (get-actor (action-actor-id action)))
      ; a bit hacky check to see if this actor has been removed already
      (define actor-removed? (empty? (actor-location-id actor)))

      (cond ((not actor-removed?)
              (define pre-action-reaction? (get-pre-action-reaction action))
              (when (not (null? pre-action-reaction?))
                (set! action pre-action-reaction?))

              (define turn-result (resolve-turn! action))

              (define post-action-reaction-from-target? (get-post-action-reaction action turn-result))
              (when (not (null? post-action-reaction-from-target?))
                ;(define action post-action-reaction-from-target?)
                (dev-note (format "-- post-action-reaction-from-target?: ~a" post-action-reaction-from-target?)))

              (dev-note (format "Turn result for ~a: ~a" (get-combatant-name (get-actor (action-actor-id action))) turn-result))
              (when (empty? (get-current-enemies))
                (dev-note (format "-- No more enemies"))
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

(define (resolve-turn! action)
  (resolve-action! action))

(define (end-combat)
  (remove-all-enemies-and-end-combat!)
  (clear-action-queue!))
