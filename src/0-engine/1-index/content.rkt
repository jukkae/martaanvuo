#lang at-exp racket

(provide
  describe-begin-traverse-action
  describe-finish-traverse-action
  describe-cancel-traverse-action
  display-location-info-card
  get-traverse-text
  )

(require racket/lazy-require)

(lazy-require ["../../1-content/narration/locations.rkt"
  (describe-begin-traverse-action
   describe-finish-traverse-action
   describe-cancel-traverse-action
   display-location-info-card
   get-traverse-text
   )])
