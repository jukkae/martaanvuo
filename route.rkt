#lang racket

(provide (all-defined-out))

(require racket/serialize)

(require "location.rkt")

(serializable-struct
 route
 ([id #:mutable] ; mutable for s11n, but should not be mutated
  [a #:mutable]
  [b #:mutable]
  [details #:mutable]))

(define (get-traverse-text route starting-location)
  
  ;(cond ((location-visited? to) ; TODO: This should be stored in the route, rather
  (cond (#t ; TODO: This should be stored in the route, rather
         (define from-name (get-location-name-from-location (route-a route)))
         (define to-name (get-location-name-from-location (route-b route)))
         (string-append "Go back to " to-name "."))
        (else
         (define from-name (get-location-name-from-location (route-a route)))
         (define to-name (get-location-name-from-location (route-b route)))
         (string-append "Go to " to-name "."))))