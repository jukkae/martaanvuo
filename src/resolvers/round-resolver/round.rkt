#lang at-exp racket

(provide on-begin-round
         on-end-round)

(require racket/lazy-require)

(require
  "round-summary.rkt"
  "action-queue.rkt"
  "fragment-handler.rkt"

  "../../actors/actor.rkt"

  "../../core/io.rkt"
  "../../core/utils.rkt"

  "../../fragments/fragment.rkt"

  "../../locations/0-types/location.rkt"
  "../../locations/routes.rkt"

  "../../state/state.rkt"

  "../../world/world.rkt"

  )


(lazy-require
 ["../../combat/combat.rkt"
  (get-combatant-name
   display-combatant-info
   display-pc-combatant-info
   end-combat!)])

(lazy-require
 ["../../state/logging.rkt"
  (set-prompt!)])

(define (on-begin-round mode)
  (case mode
    ['begin
     (current-round (add1 (current-round)))
     (when (current-show-round-summary?) (round-summary mode))
     (clear-action-queue!)

     ; does this also need to happen when 'continue?
     (define round-begin-status
       (cond ((not (null? (current-fragment-id)))
              (current-fragment-on-begin-round!))))

     ; mark location as visited w.r.t routes
     ; todo logic is shit here (should maybe happen in traversal action resolution)
     (let ([location (current-location)])
      (when (place? location)
        (for ([route-id (place-routes location)])
          (when #t ; if not hidden
            (set-route-endpoint-visited! (get-route-by-id route-id) (location-id location))
            ))
        ))
     (case round-begin-status
       ['ok 'ok]
       ['pc-dead 'pc-dead])]

    ['continue
     (when (current-show-round-summary?) (round-summary mode))
     (clear-action-queue!)]))

(define (on-end-round)
  #;(displayln "[End round]")
  (set-prompt! "") ; TODO: can be done much much earlier in the round - when should it be done?
  (define current-enemies (get-current-enemies))

  (when (and (in-combat?)
             (= (length current-enemies) 0))
    (end-combat!))
  #;(wait-for-confirm)

  (when (not (null? (current-fragment-id)))
    (current-fragment-on-end-round!)) ; TODO fragment-rounds should maybe not increase round?

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

  ; proc conditions - TODO this is currently only for PC, fix if needed!
  (define pc-conditions (actor-conditions (pc)))
  (for ([condition pc-conditions])
    (process-condition-on-end-turn (pc) condition)
    #;((condition-on-end-round! condition)) ; lambdas don't serialize, rethink this
    '())

; mark location itself as visited, as opposed to its routes
     (let ([location (current-location)])
      (when (place? location)
        (set-place-visited?! location #t)
        ))

  (if #f
      (newline) ; This is the "extra" newline that separates rounds
      '()))
