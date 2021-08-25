#lang racket

(provide resolve-action!)

(require racket/lazy-require)

(require rebellion/collection/association-list)

(require "../action.rkt")
(require "../actor.rkt")
(require "../checks.rkt")
(require "../condition.rkt")
(require "../io.rkt")
(require "../item.rkt")
(require "../locations/location.rkt")
(require "../pc.rkt")
(require "../locations/route.rkt")
(require "../state/state.rkt")
(require "../state/logging.rkt")
(require "../stance.rkt")
(require "../status.rkt")
(require "../utils.rkt")
(require "../world.rkt")

(require "../round-resolver/event.rkt"
         "../round-resolver/simulation.rkt"
         "../round-resolver/timeline.rkt")

(require
  "combat-actions.rkt"
  "downtime-actions.rkt"
  "special-actions.rkt"
  "traverse-action.rkt")


(lazy-require
 ["state/combat.rkt"
  (get-combatant-name
   display-combatant-info
   display-pc-combatant-info
   add-combat-flag
   )])



(lazy-require
 ["../round-resolver/event-handler.rkt"
  (handle-interrupting-event!
   )])


; what all should this return?
(define (resolve-action! action)
  (when (actor-alive? (action-actor action))
    (define result
      (case (action-symbol action)
        ; "special" actions first
        ['end-run (resolve-special-action! action)]
        ['back-off (resolve-special-action! action)]
        ['win-game (resolve-special-action! action)]
        ['skip (resolve-special-action! action)]
        
        ['go-to-location (resolve-go-to-action! action)]
        ['traverse (resolve-traverse-action! action)]
        ['cancel-traverse (resolve-cancel-traverse-action! action)]
      
      
        ; the rest
        ['melee (resolve-melee-action! action)]
        ['shoot (resolve-shoot-action! action)]
        ['forage (resolve-forage-action! action)]
        ['sleep (resolve-sleep-action! action)]
      
        ['flee (resolve-flee-action! action)]
        ['break-free (resolve-break-free-action! action)]

        ['anklebreaker (resolve-anklebreaker-action! action)]
        ['pull-under (resolve-pull-under-action! action)]
        ['release-grip 'grip-released]

        ['go-to-engaged (resolve-go-to-engaged-action! action)]
        ['go-to-close (resolve-go-to-close-action! action)]


        ['inflict-status (resolve-inflict-status-action! action)]

        ['modify-status (resolve-modify-status-action! action)]

        ['inflict-condition (resolve-inflict-condition-action! action)]

        [else
         (error (string-append "resolve-action!: unknown action type " (symbol->string (action-symbol action))))]))


    ; what a hack
    (when (timeline? result)
      (handle-pc-action-interrupted! result)
      (set-pending-action! action (timeline-duration result))
      (set! result 'interrupted))

    result
    ))


(define (set-pending-action! action elapsed-time)
  (define time-left (- (action-duration action) elapsed-time))
  (define pending-action action)
  (set-action-duration! pending-action time-left)
  (set-action-details! pending-action (append-element (action-details pending-action) 'pending))
  (current-pending-action pending-action))






(define (handle-pc-action-interrupted! timeline)
  (define interrupting-events
    (filter
     (λ (event) (event-interrupting? event))
     (timeline-events timeline)))
  
  (cond ((eq? (length interrupting-events) 1)
         (define event (car interrupting-events))
         (handle-interrupting-event! event)
         )
        (else
         (dev-note "handle-pc-action-interrupted!: unexpected amount of interrupting events.")))
  )



