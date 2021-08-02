#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(lazy-require
 ["location.rkt"
  (get-location-name-from-location)])

(lazy-require
 ["place.rkt"
  (place-id)])

(serializable-struct
 route
 ([id #:mutable] ; mutable for s11n, but should not be mutated
  [a #:mutable]
  [b #:mutable]
  [details #:mutable]
  [actors #:mutable]))

(define (get-traverse-text route starting-location)
  (define direction
    (cond ((eq? (place-id starting-location)
                (place-id (route-a route)))
           'a-to-b)
          ((eq? (place-id starting-location)
                (place-id (route-b route)))
           'b-to-a)))

  ; (string-append "Go back to " to-name ".")
  (case direction
    ['a-to-b
     (define from-name (get-location-name-from-location (route-a route)))
     (define to-name (get-location-name-from-location (route-b route)))
     (string-append "Go to " to-name ".")]
    ['b-to-a
     (define from-name (get-location-name-from-location (route-b route)))
     (define to-name (get-location-name-from-location (route-a route)))
     (string-append "Go to " to-name ".")]))