#lang at-exp racket

(provide
 (all-defined-out))

(require racket/lazy-require)

(require
  "../../actors/actor.rkt"
  "../../core/checks.rkt"
  "../../core/io.rkt"
  "../../core/utils.rkt"
  "../../state/state.rkt"
  )


(lazy-require
 ["../round-resolver/event-handler.rkt"
  (handle-interrupting-event!
   )])

(lazy-require
 ["../../combat/combat.rkt"
  (get-combatant-name
   display-combatant-info
   display-pc-combatant-info
   add-combat-flag
   )])

(lazy-require
 ["../../locations/locations.rkt"
  (describe-begin-traverse-action
   describe-finish-traverse-action
   describe-cancel-traverse-action
   location-on-enter!
   )])

(define (resolve-sleep-action! action)
  (p "Otava makes camp.")
  'ok)

; just a skill check in a fancy coat
(define (resolve-forage-action! action)
  'ok)
