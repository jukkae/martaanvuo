#lang at-exp typed/racket

(provide (all-defined-out))

(require
  "../../2-core/maybe.rkt"
  "../../3-types/location-ids.rkt"
  "../../actors/actor.rkt"
  "../../items/item.rkt"
  )

(struct
  location
  ([id : LocationId]
   [type : (Maybe Symbol)]
   [details : (Listof Symbol)]
   [actors : (Listof actor)]
   [items : (Listof (U item Symbol))]
   [features : (Listof Symbol)]
   [tags : (Listof Symbol)])
  #:prefab
  #:mutable
  #:constructor-name location*)

(define (add-actor-to-location! [location : location] [actor : actor])
  (set-location-actors! location (cons actor (location-actors location))))

(define (remove-actor-from-location! [location : location] [actor : actor])
  (set-location-actors! location (remove actor (location-actors location))))

(define (add-item-to-location! [location : location] [item : (U item Symbol)])
  (set-location-items! location (cons item (location-items location))))

(define (remove-item-from-location! [location : location] [item : (U item Symbol)])
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



(struct place
  location
  ([routes : (Listof RouteId)]
   [visited? : Boolean]
   [actions-provided : (Listof Symbol)]
   [shortname : String])

  #:constructor-name place*
  #:mutable
  #:prefab)

(define *number-of-places* : Natural 0)

(: make-place (->* ()
                   (#:id (U Symbol Natural)
                    #:type Symbol
                    #:details (Listof Symbol)
                    #:actors (Listof actor)
                    #:items (Listof (U item Symbol))
                    #:features (Listof Symbol)
                    #:tags (Listof Symbol)
                    #:visited? Boolean
                    #:actions-provided (Listof Symbol)
                    #:shortname String)
                   place))
(define (make-place
         #:id [id *number-of-places*]
         #:type [type '()]
         #:details [details '()]
         #:actors [actors '()]
         #:items [items '()]
         #:features [features '()]
         #:tags [tags '()]
         #:routes [routes '()]
         #:visited? [visited? #f]
         #:actions-provided [actions-provided '()]
         #:shortname [shortname ""])

  (set! *number-of-places* (add1 *number-of-places*))


  (: id-symbol Symbol)
  (define id-symbol
    (match id
     [(? symbol? id) id]
     [Natural (string->symbol (format "place-~a" id))]
     ))

  (place* id-symbol
          type
          details
          actors
          items
          features
          tags
          routes
          visited?
          actions-provided
          shortname))

