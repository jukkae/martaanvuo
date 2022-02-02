#lang at-exp racket

(provide (all-defined-out))

(require racket/serialize)

(require "location.rkt")

(serializable-struct
 route
 location
 ([a #:mutable]
  [b #:mutable])
 #:transparent
 #:constructor-name route*)

(define
  (make-route id
              a
              b
              #:type [type '()]
              #:details [details '()]
              #:actors [actors '()]
              #:items [items '()]
              #:features [features '()]
              #:tags [tags '()])

  (route* id
          type
          details
          actors
          items
          features
          tags
          a
          b))



(define (route-traversed? route)
  (location-has-detail? route 'traversed))

(define (route-a-visited? route)
  (location-has-detail? route 'a-visited))

(define (route-b-visited? route)
  (location-has-detail? route 'b-visited))

(define (set-route-traversed! route)
  (add-detail-to-location! route 'traversed))

(define (route-fully-known? route)
  (and (route-a-visited? route)
       (route-b-visited? route)
       (route-traversed? route)))
