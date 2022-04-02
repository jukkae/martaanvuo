#lang at-exp racket

(provide on-begin-round
         on-end-round)

(require racket/lazy-require)

(require
  "action-queue.rkt"
  "fragment-handler.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"

  "../../3-types/actor.rkt"
  "../../3-types/location.rkt"

  "../../4-systems/actors/actor.rkt"
  "../../4-systems/fragments/fragment.rkt"
  "../../4-systems/locations/routes.rkt"
  "../../4-systems/world/world.rkt"

  "../../7-state/state/state.rkt"

  "../../../1-content/narration/round-summary.rkt"
  )


(lazy-require ["../../6-combat/combat.rkt"
  (end-combat!
   )])

(lazy-require ["../../6-combat/narration.rkt"
  (get-combatant-name
   display-combatant-info
   display-pc-combatant-info
   )])

(lazy-require ["../../7-state/state/logging.rkt"
  (set-prompt!)])

(define (on-begin-round mode)
  (case mode
    ['begin
 (current-round (add1 (current-round)))
 (when (current-show-round-summary?) (round-summary mode))
 (clear-action-queue!)

 (define round-begin-status
   (cond ((not (null? (current-fragment-id)))
          (current-fragment-on-begin-round!))))

 ; mark location as visited w.r.t routes
 (let ([location (current-location)])
   (when (place? location)
     (for ([route-id (place-routes location)])
       (when #t ; if not hidden
         (set-route-endpoint-visited! (get-route-by-id route-id) (location-id location))
         ))
     ))
 #;(case round-begin-status
     ['ok 'ok]
     ['pc-dead 'pc-dead])
 'ok]

    ['continue
     (when (current-show-round-summary?) (round-summary mode))
     (clear-action-queue!)]))

(define (on-end-round)
  (set-prompt! "")
  (define current-enemies (get-current-enemies))

  (when (and (in-combat?)
             (= (length current-enemies) 0))
    (end-combat!))

  (when (not (null? (current-fragment-id)))
    (current-fragment-on-end-round!))

  ; remove statuses
  (for ([enemy (get-current-enemies)])
    (define name (get-combatant-name enemy))
    (when (not (null? (actor-statuses enemy)))
      (notice (format "~a: removed statuses:" name))
      (for ([status (actor-statuses enemy)])
        (displayln status))
      (decrement-actor-status-lifetimes! enemy)))

  (for ([enemy (get-current-enemies)])
    (define name (get-combatant-name enemy))
    (when (not (null? (actor-statuses enemy)))
      (define name (get-combatant-name enemy))
      (define description (~s (actor-statuses enemy)))

      (define description-prefix
        (format "[~a: removed statuses: " name))
      (define description-suffix "]")
      (decrement-actor-status-lifetimes! enemy)))

  ; urgh
  (when (not (null? (actor-statuses (pc))))
    (define name (get-combatant-name (pc)))
    (define description (~s (actor-statuses (pc))))

    (define description-prefix
      (format "[~a: removed statuses" name))
    (define description-suffix "]")
    (decrement-actor-status-lifetimes! (pc)))

  (for ([condition (actor-conditions (pc))])
    (condition-on-end-round! condition (actor-id (pc))))

  (for ([enemy (get-current-enemies)])
    (for ([condition (actor-conditions enemy)])
      (condition-on-end-round! condition (actor-id enemy))))

  ; mark location itself as visited, as opposed to its routes
  (let ([location (current-location)])
    (when (place? location)
      (set-place-visited?! location #t)
      ))

  (if #f
      (newline) ; This is the "extra" newline that separates rounds
      '()))
