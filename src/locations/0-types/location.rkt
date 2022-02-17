#lang at-exp typed/racket

(provide (all-defined-out))

(require "../../actors/actor.rkt"
         "../../items/item.rkt")

(struct
  location
  ([id : Symbol]
   [type : Symbol]
   [details : (Listof Symbol)]
   [actors : (Listof actor)]
   [items : (Listof item)]
   [features : (Listof Symbol)]
   [tags : (Listof Symbol)])
  #:prefab
  #:mutable
  #:constructor-name location*)

(define (add-actor-to-location! [location : location] [actor : actor])
  (set-location-actors! location (cons actor (location-actors location))))

(define (remove-actor-from-location! [location : location] [actor : actor])
  (set-location-actors! location (remove actor (location-actors location))))

(define (add-item-to-location! [location : location] [item : item])
  (set-location-items! location (cons item (location-items location))))

(define (remove-item-from-location! [location : location] [item : item])
  (set-location-items! location (remove item (location-items location))))

(define (add-feature-to-location! [location : location] [feature : Symbol])
  (set-location-features! location (cons feature (location-features location))))

(define (remove-feature-from-location! [location : location] [feature : Symbol])
  (set-location-features! location (remove feature (location-features location))))

(define (location-has-feature? [location : location] [feature : Symbol])
  (memq feature (location-features location)))

(define (add-detail-to-location! [location : location] [detail : Symbol])
  (when (not (location-has-detail? location detail))
    (set-location-details! location (cons detail (location-details location)))))

(define (remove-detail-from-location! [location : location] [detail : Symbol])
  (when (location-has-detail? location detail)
    (set-location-details! location (remove detail (location-details location)))))

(: location-has-detail? (-> location Symbol Boolean))
(define (location-has-detail? location detail)
  (if (findf (λ (location-detail) (eq? detail location-detail))
             (location-details location))
      #t
      #f))

(define (location-has-tag? [location : location] [tag : Symbol])
  (memq tag (location-tags location)))





; (define-type PlaceId Symbol) TODO: do this for all IDs
(struct place
  location
  ([routes : (Listof route)]
   [on-enter-symbol : Symbol] ; symbol, because lambdas cannot be easily serialized -> fix!
   [visited? : Boolean]
   [actions-provided : (Listof Symbol)]
   [shortname : String])

  #:constructor-name place*
  #:mutable
  #:prefab)

(define *number-of-places* 0)

(define (get-numeric-id) *number-of-places*)

(define (make-place
         #:id [id (get-numeric-id)]
         #:type [type '()]
         #:details [details '()]
         #:actors [actors '()]
         #:items [items '()]
         #:features [features '()]
         #:tags [tags '()]
         #:routes [routes '()]
         #:on-enter-symbol [on-enter-symbol '()]
         #:visited? [visited? #f]
         #:actions-provided [actions-provided '()]
         #:shortname [shortname ""])

  (set! *number-of-places* (add1 *number-of-places*))

  (place* id
          type
          details
          actors
          items
          features
          tags
          routes
          on-enter-symbol
          visited?
          actions-provided
          shortname))







(struct route
  location
  ([a : Symbol]  ; TODO: should be place-id
   [b : Symbol])
  #:mutable
  #:prefab
  #:constructor-name route*)

(define (make-route id
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
