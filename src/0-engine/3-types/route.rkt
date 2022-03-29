#lang at-exp typed/racket

(provide (all-defined-out))

(require
  "location.rkt"

  "../2-core/maybe.rkt"
  "../3-types/location-ids.rkt"
  "../3-types/actor.rkt"
  "../3-types/item.rkt"
  )

(struct route
  location
  ([a : PlaceId]
   [b : PlaceId]
   [traverse-time : Natural])
  #:mutable
  #:prefab
  #:constructor-name route*)

(define *number-of-routes* 0)

(: make-route (->* (PlaceId PlaceId Natural)
                   (#:id RouteId
                    #:type (Maybe Symbol)
                    #:details (Listof Symbol)
                    #:actors (Listof actor)
                    #:items (Listof item)
                    #:features (Listof Symbol)
                    #:tags (Listof Symbol))
                   route))
(define (make-route a
                    b
                    traverse-time
                    #:id [id *number-of-routes*]
                    #:type [type '()]
                    #:details [details '()]
                    #:actors [actors '()]
                    #:items [items '()]
                    #:features [features '()]
                    #:tags [tags '()])

  (set! *number-of-routes* (add1 *number-of-routes*))

  (: id-symbol Symbol)
  (define id-symbol
    (match id
     [Natural (string->symbol (format "route-~a" id))]
     [Symbol id]))

  (route* id-symbol
          type
          details
          actors
          items
          features
          tags
          a
          b
          traverse-time))

(: route-traversed? (->* (route) () Boolean))
(define (route-traversed? route)
  (location-has-detail? route 'traversed))

(: route-a-visited? (->* (route) () Boolean))
(define (route-a-visited? route)
  (location-has-detail? route 'a-visited))

(: route-b-visited? (->* (route) () Boolean))
(define (route-b-visited? route)
  (location-has-detail? route 'b-visited))

(: set-route-traversed! (->* (route) () Void))
(define (set-route-traversed! route)
  (add-detail-to-location! route 'traversed))

(: route-fully-known? (->* (route) () Boolean))
(define (route-fully-known? route)
  (and (route-a-visited? route)
       (route-b-visited? route)
       (route-traversed? route)))
