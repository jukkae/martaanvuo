#lang racket

(provide on-begin-round
         on-end-round)

(require racket/lazy-require)

(require "../actor.rkt"
         "../condition.rkt"
         "../fragment.rkt"
         "../round-summary.rkt"
         "../situation.rkt")

(require "action-queue.rkt"
         "fragment-handler.rkt")


(lazy-require
 ["state/combat.rkt"
  (get-combatant-name
   display-combatant-info
   display-pc-combatant-info
   end-combat!
   )])

(define (on-begin-round mode)
  (case mode
    ['begin
     (set-situation-round! *situation* (add1 (situation-round *situation*)))
     (round-summary *situation*)
     (clear-action-queue!)
     (when (not (null? (situation-current-fragment-id *situation*)))
       (current-fragment-on-begin-round!))]
    
    ['continue
     (round-summary *situation*)
     (clear-action-queue!)]))

(define (on-end-round)
  #;(displayln "[End round]")
  (set-prompt! "") ; TODO: can be done much much earlier in the round - when should it be done?
  (define current-enemies (get-current-enemies))

  (when (and (in-combat?)
             (= (length current-enemies) 0))
    (end-combat!))
  #;(wait-for-confirm)
  
  (when (not (null? (situation-current-fragment-id *situation*)))
    (current-fragment-on-end-round!)) ; TODO fragment-rounds should maybe not increase round?

  ; remove statuses
  (for ([enemy (get-current-enemies)])
    (define name (get-combatant-name enemy))
    (when (not (null? (actor-statuses enemy)))
      (displayln (string-append "[" name ": removed statuses:]"))
      (for ([status (actor-statuses enemy)])
        (displayln status))
      (decrement-actor-status-lifetimes! enemy)))

  (for ([enemy (get-current-enemies)])
    (define name (get-combatant-name enemy))
    (when (not (null? (actor-statuses enemy)))
      (define name (get-combatant-name enemy))
      (define description (~s (actor-statuses enemy)))
    
      (define description-prefix
        (string-append "[" name ": removed statuses: "))
      (define description-suffix "]")
      (decrement-actor-status-lifetimes! enemy)))

  ; urgh
  (when (not (null? (actor-statuses (situation-pc *situation*))))
    (define name (get-combatant-name (situation-pc *situation*)))
    (define description (~s (actor-statuses (situation-pc *situation*))))
    
    (define description-prefix
      (string-append "[" name ": removed statuses: "))
    (define description-suffix "]")
    (decrement-actor-status-lifetimes! (situation-pc *situation*)))

  
  ; proc conditions - TODO this is currently only for PC, fix if needed!
  (define pc-conditions (actor-conditions (pc)))
  (for ([condition pc-conditions])
    (process-condition-on-end-turn (pc) condition)
    #;((condition-on-end-round! condition)) ; lambdas don't serialize, rethink this
    '()
    )
  
  
  #;(newline) ; This is the "extra" newline that separates rounds
  #;(wait-for-confirm)
  )