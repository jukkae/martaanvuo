#lang at-exp racket

(provide resolve-action!)

(require racket/lazy-require)

(require
  "../../0-api/rules-resolution-runtime.rkt"

  "../../0-api/types.rkt"
  "../../1-index/state.rkt"

  "../../2-core/core.rkt"

  "../../4-systems/timelines.rkt"
  "../../4-systems/actors/actor.rkt"
  "../../4-systems/world/world.rkt"
  "../../4-systems/simulation.rkt"

  )

(lazy-require ["../1-round-resolver/event-handler.rkt"
  (handle-interrupting-event!
   )])

(lazy-require ["../../4-systems/locations/locations.rkt"
  (move-pc-to-location!
   )])

(lazy-require ["../../7-state/mutators.rkt"
  (remove-all-enemies-and-end-combat!
   get-enemies-at-range
   )])


(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (rules-to-lambda rules)
  `(λ () ,@rules))

; action-result is either a timeline, a symbol, or void
(define (resolve-action! action)
  (when (actor-alive? (get-actor (action-actor-id action)))
    (define result 'not-resolved)

    (when (and (pc-actor? (get-actor (action-actor-id action)))
               (not (pending? action)))


      (define on-before-rules (action-on-before-rules action))
      (when (not (empty? on-before-rules))
        (when (not (procedure? on-before-rules))
          (set! on-before-rules (rules-to-lambda on-before-rules)))
        (define resolution-result ((eval on-before-rules ns)))
        (set! result resolution-result)))


    (when (timeline? result)
      (handle-pc-action-interrupted! result)

      (when (not (equal? (action-symbol) 'rest))
        (set-pending-action! action (- (action-duration action)
                                       (timeline-duration result))))

      (set! result 'interrupted))

    (when (not (equal? result 'interrupted))

      (define rules (action-resolution-rules action))

      (cond
        [(not (empty? rules))
         (when (not (procedure? rules))
           (set! rules (rules-to-lambda rules)))
         (define resolution-result ((eval rules ns)))

         (when (not (or (void? resolution-result) (empty? resolution-result)))
           (set! result resolution-result))]
        #; [else ; TODO:
         (dev-note (format "Empty rules for action ~a" (action-symbol action)))]
      )

      (when (not (equal? result 'time-passing-handled))
        (define duration (action-duration action))
        (define tl (advance-time-until-next-interesting-event! duration #f))
        (process-timeline! tl))
      )



    (when (and (pc-actor? (get-actor (action-actor-id action)))
               (not (equal? result 'interrupted)))

      (define on-after-rules (action-on-after-rules action))
      (when (not (empty? on-after-rules))
        (when (not (procedure? on-after-rules))
          (set! on-after-rules (rules-to-lambda on-after-rules)))
        (define resolution-result ((eval on-after-rules ns)))

        (when (not (void? resolution-result))
          (set! result resolution-result)
          )
        ))

    #;(wait-for-confirm)

    result))


(define (set-pending-action! action time-left)
  (define pending-action action)
  (set-action-duration! pending-action time-left)
  (set-action-details! pending-action (append-element (action-details pending-action) 'pending))
  (current-pending-action pending-action))


(define (handle-pc-action-interrupted! timeline)
  (define interrupting-events
    (filter
     (λ (event) (event-interrupting? event))
     (timeline-events timeline)))
  (cond ((equal? (length interrupting-events) 1)
         (define event (car interrupting-events))
         (handle-interrupting-event! event)
         )
        (else
         (dev-note "handle-pc-action-interrupted!: unexpected amount of interrupting events.")))
  )
