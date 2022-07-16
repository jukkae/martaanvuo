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
  get-location-choices)
(lazy-require ["../../1-content/world/locations/get-location-choices.rkt" (
  get-location-choices
  )])

(provide
  on-begin-playthrough!
  on-begin-run
  on-begin-recurse-run
  on-end-run
  on-begin-life
  on-end-life)
(lazy-require ["../../1-content/martaanvuo.rkt" (
  on-begin-playthrough!
  on-begin-run
  on-begin-recurse-run
  on-end-run
  on-begin-life
  on-end-life
  )])
