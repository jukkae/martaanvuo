#lang racket

(provide resolve-cancel-traverse-action!)

(require racket/lazy-require)

(require "../action.rkt"
         "../location.rkt"
         "../state/state.rkt")

(lazy-require
 ["locations.rkt"
  (describe-begin-traverse-action
   describe-finish-traverse-action
   describe-cancel-traverse-action
   location-on-enter!
   )])

(define (resolve-cancel-traverse-action! action)
  (reset-pending-action!)
  (move-pc-to-location! (action-target action))

  (describe-cancel-traverse-action action)
  (display-location-info-card (current-location))
  (when (not (null? (location-items (action-target action))))
    (pick-up-items!))
  'ok)