#lang at-exp racket

(provide describe-begin-traverse-action
         describe-finish-traverse-action
         describe-cancel-traverse-action
         display-location-info-card
         get-traverse-text
         route-shortname)

(require "../../0-engine/0-api/api.rkt")
(require "places.rkt"
         "routes.rkt")

(define (display-location-info-card location [title "Location"])
  (cond
    [(Place? location) (display-place-info-card location)]
    [(route? location) (display-route-info-card location)]
    [else (dev-note (format "location-info-card: unknown location: ~a" location))]))
