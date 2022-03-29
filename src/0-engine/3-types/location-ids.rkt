#lang typed/racket

(provide (all-defined-out))

(define-type PlaceId Symbol)
(define-type RouteId Symbol)
(define-type LocationId (U PlaceId RouteId))
