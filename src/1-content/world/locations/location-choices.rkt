#lang at-exp racket

(provide (all-defined-out))

(require
  "../../../0-engine/0-api/api.rkt")

(require
  "perimeter.rkt")

(define (get-location-choices location)
  (case location-id
   ['perimeter (get-perimeter-choices)]))