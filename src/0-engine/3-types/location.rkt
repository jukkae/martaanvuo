#lang at-exp typed/racket

(provide (all-defined-out))

(require
  "location-ids.rkt"
  "actor.rkt"
  "item.rkt"

  "../2-core/maybe.rkt"
  "../3-types/choice.rkt"
  "../3-types/light-levels.rkt"
  "../4-systems/actors/actor.rkt"
  )

(define-type LocationType (U 'int 'ext))

(struct
  location
  ([id : LocationId]
   [type : (Maybe LocationType)]
   [details : (Listof Symbol)]
   [actors : (Listof actor)]
   [items : (Listof (U item Symbol))]
   [features : (Listof Symbol)]
   [tags : (Listof Symbol)]
   [light-level : (U LightLevel 'natural)]
   [encounter-types : (Listof Symbol)]
   )
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
  (if (findf (λ (location-detail) (equal? detail location-detail))
             (location-details location))
      #t
      #f))

(define (location-has-tag? [location : location] [tag : Symbol])
  (memq tag (location-tags location)))

