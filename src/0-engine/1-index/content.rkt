#lang at-exp racket

(require racket/lazy-require)

(provide
  describe-begin-traverse-action
  describe-finish-traverse-action
  describe-cancel-traverse-action
  display-location-info-card
  get-traverse-text
  )
(lazy-require ["../../1-content/narration/locations.rkt"
  (describe-begin-traverse-action
   describe-finish-traverse-action
   describe-cancel-traverse-action
   display-location-info-card
   get-traverse-text
   )])

(provide
  get-perimeter-choices)
(lazy-require ["../../1-content/world/locations/perimeter.rkt"
  (get-perimeter-choices
   )])

(provide
  make-new-world)
(lazy-require ["../../1-content/world/world.rkt" (
  make-new-world
  )])

(provide
  on-begin-run
  on-end-run)
(lazy-require ["../../1-content/gameplay-transitions.rkt" (
  on-begin-run
  on-end-run
  )])