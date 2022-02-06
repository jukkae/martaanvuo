#lang at-exp racket


(require
  "ai.rkt"
  "action-queue.rkt"

  "../action-resolver/action-resolver.rkt"

  "../../actions/action.rkt"

  "../../world/world.rkt"

  "../../core/io.rkt"
  "../../core/utils.rkt"

  "../../pc/pc.rkt"

  "../../state/state.rkt")



(provide resolve-turns!)
(define (resolve-turns!)
  (let/ec end-round-early
    (when (all-actions-of-type? action-queue 'flee)
      (p "Otava turns her back to flee and crawls under a bush to hide. She waits a while. Nothing seems to be following her.")
      (award-xp! 1)
      (end-combat)
      (end-round-early))
    (for ([action action-queue])

      (define actor (action-actor action))

      (define pre-action-reaction? (get-pre-action-reaction action))
      (when (not (null? pre-action-reaction?))
        (set! action pre-action-reaction?))

      (define turn-result (resolve-turn! action))

      ; todo
      (define post-action-reaction-from-target? (get-post-action-reaction action turn-result))
      (when (not (null? post-action-reaction-from-target?))
        ;(define action post-action-reaction-from-target?)
        (dev-note (format "-- post-action-reaction-from-target?: ~a" post-action-reaction-from-target?)))

      (dev-note (format "Turn result: ~a" turn-result))

      (when (eq? turn-result 'escape-from-combat)
        (remove-actor-from-its-current-location! actor)
        (dev-note (format "~a" (get-current-enemies)))
        (when (empty? (get-current-enemies))
          (dev-note (format "-- No more enemies"))
          (set! turn-result 'end-combat)
          )
        )

      (case turn-result

        ['pc-dead
         (end-round-early)]

        ['end-combat
         (end-combat)
         (end-round-early)]

        ; TODO: As always, description belongs in the action
        ['grip-released
         (p "The Grabberkin's hands let go of Otava's ankles and disappear under the moss.")
         (award-xp! 3 "for surviving an encounter with a Grabberkin")
         (remove-enemy actor)]))))

(define (resolve-turn! action)
  (resolve-action! action))

(define (end-combat)
  (remove-all-enemies-and-end-combat!)
  (clear-action-queue!))
