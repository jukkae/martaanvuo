#lang at-exp typed/racket

(provide (all-defined-out))

(require "location.rkt"

         "../2-core/maybe.rkt"
         "../3-types/actor.rkt"
         "../3-types/item.rkt"
         "../3-types/light-levels.rkt"
         "../3-types/location-ids.rkt")

(struct route
        location
        ([a : PlaceId] [b : PlaceId]
                       [traverse-time : Natural]
                       [one-directional? : Boolean]
                       [hidden? : Boolean]
                       [descr-from-a : (Maybe String)]
                       [descr-from-b : (Maybe String)])
  #:mutable
  #:prefab
  #:constructor-name route*)

(define *number-of-routes* 0)

(: make-route
   (->* (PlaceId PlaceId Natural)
        (Boolean Boolean
                 #:id RouteId
                 #:type (Maybe LocationType)
                 #:size (Maybe LocationSize)
                 #:details (Listof Symbol)
                 #:actors (Listof actor)
                 #:items (Listof item)
                 #:features (Listof Symbol)
                 #:hidden-features (Listof Symbol)
                 #:tags (Listof Symbol)
                 #:encounter-types (Listof Symbol)
                 #:descr-from-a (Maybe String)
                 #:descr-from-b (Maybe String))
        route))
(define (make-route a
                    b
                    traverse-time
                    (one-directional? #f)
                    (hidden? #f)
                    #:id [id *number-of-routes*]
                    #:type [type '()]
                    #:size [size '()]
                    #:details [details '()]
                    #:actors [actors '()]
                    #:items [items '()]
                    #:features [features '()]
                    #:hidden-features [hidden-features '()]
                    #:tags [tags '()]
                    #:encounter-types [encounter-types '()]
                    #:descr-from-a [descr-from-a ""]
                    #:descr-from-b [descr-from-b ""])

  (set! *number-of-routes* (add1 *number-of-routes*))

  (: id-symbol Symbol)
  (define id-symbol
    (match id
      [Natural (string->symbol (format "route-~a" id))]
      [Symbol id]))

  (define zones '())

  (route* id-symbol
          type
          size
          details
          actors
          items
          features
          hidden-features
          zones
          tags
          encounter-types
          a
          b
          traverse-time
          one-directional?
          hidden?
          descr-from-a
          descr-from-b))

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
  (and (route-a-visited? route) (route-b-visited? route) (route-traversed? route)))
