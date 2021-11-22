#lang racket

(provide route-other-end-from
         route-shortname
         set-route-endpoint-visited!)

(require "place.rkt"
         "route.rkt"
         "../core/api.rkt")


(define (route-other-end-from route start-location)
  (define start
    (cond ((eq? (location-id start-location)
                (location-id (route-a route)))
           'a)
          ((eq? (location-id start-location)
                (location-id (route-b route)))
           'b)))
  (define endpoint
    (case start
      ['a (route-b route)]
      ['b (route-a route)]))
  endpoint)



(define (set-route-endpoint-visited! route location)
  (define endpoint
    (cond ((eq? (location-id location)
                (location-id (route-a route)))
           'a)
          ((eq? (location-id location)
                (location-id (route-b route)))
           'b)))
  (case endpoint
    ['a (add-detail-to-location! route 'a-visited)]
    ['b (add-detail-to-location! route 'b-visited)]))


(define (route-place-known? route place)
  (define endpoint
    (cond ((eq? (location-id place)
                (location-id (route-a route)))
           'a)
          ((eq? (location-id place)
                (location-id (route-b route)))
           'b)))
  (case endpoint
    ['a (place-visited? (route-a route))]
    ['b (place-visited? (route-b route))])
  )

(define (route-shortname route)
  (define direction (get-pending-traverse-direction))

  (define startpoint
    (case direction
      ['a-to-b (route-a route)]
      ['b-to-a (route-b route)]))
  (define endpoint
    (case direction
      ['a-to-b (route-b route)]
      ['b-to-a (route-a route)]))

  
  
  (cond ((route-fully-known? route)
         (string-append "En route: "
                        (place-shortname startpoint)
                        " – "
                        (place-shortname endpoint)
                        " "))
        (else
         (string-append "En route: "
                        (place-shortname startpoint)
                        " – "
                        "???"
                        " "))))