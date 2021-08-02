#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "utils.rkt")

(lazy-require
 ["location.rkt"
  (get-location-name-from-location)])

(lazy-require
 ["place.rkt"
  (place-id
   place-visited?)])

(serializable-struct
 route
 ([id #:mutable] ; mutable for s11n, but should not be mutated
  [a #:mutable]
  [b #:mutable]
  [details #:mutable]
  [actors #:mutable]))

(define (get-traverse-text route starting-location)

  (cond ((route-fully-known? route)
  
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
        (else
         "ROUTE PARTLY UNKNOWN")))

(define (set-route-endpoint-visited! route location)
  (define endpoint
    (cond ((eq? (place-id location)
                (place-id (route-a route)))
           'a)
          ((eq? (place-id location)
                (place-id (route-b route)))
           'b)))
  (case endpoint
    ['a (set-route-details! route
                            (append-element (route-details route) 'a-visited))]
    ['b (set-route-details! route
                            (append-element (route-details route) 'b-visited))]))

(define (route-traversed? route)
  (memq 'traversed (route-details route)))

(define (route-a-visited? route)
  (memq 'a-visited (route-details route)))

(define (route-b-visited? route)
  (memq 'b-visited (route-details route)))

(define (route-place-known? route place)
  (define endpoint
    (cond ((eq? (place-id place)
                (place-id (route-a route)))
           'a)
          ((eq? (place-id place)
                (place-id (route-b route)))
           'b)))
  (case endpoint
    ['a (place-visited? (route-a route))]
    ['b (place-visited? (route-b route))])
  )

(define (set-route-traversed! route)
  (set-route-details! route (append-element (route-details route) 'traversed)))

(define (route-fully-known? route)
  (and (route-a-visited? route)
       (route-b-visited? route)
       (route-traversed? route)))