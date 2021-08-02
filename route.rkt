#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "utils.rkt")

(lazy-require
 ["location.rkt"
  (get-location-name-from-location
   location-id)])

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



(define (get-traverse-text route start-location)
  (define direction
    (cond ((eq? (place-id start-location)
                (place-id (route-a route)))
           'a-to-b)
          ((eq? (place-id start-location)
                (place-id (route-b route)))
           'b-to-a)))

  (define (get-route-short-description)
    (case (location-id start-location)
      ['perimeter
       (case (location-id (route-other-end-from route start-location))
         ['magpie-hill "Magpie and the rocky slope."]
         ['martaanvuo-swamp "Ants and the swamp."]
         [else (string-append "< unknown location: " (symbol->string (location-id (route-other-end-from route start-location))) " >")])]
      [else
       (displayln "get-route-short-description: unknown location id:")
       (displayln (location-id start-location))
       "< unknown location >"]))
  
  (cond ((route-fully-known? route)
         ; (string-append "Go back to " to-name ".")
         (case direction
           ['a-to-b
            (define to-name (get-location-name-from-location (route-b route)))
            (string-append "Go to " to-name ".")]
           ['b-to-a
            (define to-name (get-location-name-from-location (route-a route)))
            (string-append "Go to " to-name ".")]))
        (else
         (get-route-short-description))))

(define (route-other-end-from route start-location)
  (define start
    (cond ((eq? (place-id start-location)
                (place-id (route-a route)))
           'a)
          ((eq? (place-id start-location)
                (place-id (route-b route)))
           'b)))
  (define endpoint
    (case start
    ['a (route-b route)]
    ['b (route-a route)]))
  endpoint)



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