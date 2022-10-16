#lang at-exp typed/racket

(provide (all-defined-out))

(require
  "location-ids.rkt"
  "actor.rkt"
  "item.rkt"
  "zone.rkt"

  "../2-core/maybe.rkt"
  "../3-types/choice.rkt"
  "../3-types/clue.rkt"
  "../3-types/light-levels.rkt"
  "../4-systems/actors/actor.rkt"
  )

(define-type LocationType (U 'int 'ext))
(define-type LocationSize (U 'container 'small 'large))

(struct
  location
  ([id : LocationId]
   [type : (Maybe LocationType)]
   [size : (Maybe LocationSize)]
   [details : (Listof Symbol)]
   [actors : (Listof actor)]
   ; TODO: FIXME: items and features – interactibles – should be in 'stacks' or 'zones', the player can be 'at' one of the stacks or at none of them, but is not 'in' any
   [items : (Listof (U item Symbol))]
   [features : (Listof Symbol)]
   [hidden-features : (Listof Symbol)]
   [zones : (Listof Zone)]

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

(define (find-item [location : location] [id : Symbol])
  (define items (location-items location))
  (findf (λ ([a : (U item Symbol)])
           (if (item? a)
               (equal? (item-id a) id)
               (equal? a id)))
         items))

(define (add-feature-to-location! [location : location] [feature : Symbol])
  (set-location-features! location (cons feature (location-features location))))

(define (remove-feature-from-location! [location : location] [feature : Symbol])
  (set-location-features! location (remove feature (location-features location))))

(define (location-has-feature? [location : location] [feature : Symbol])
  (memq feature (location-features location)))

(define (add-hidden-feature-to-location! [location : location] [hidden-feature : Symbol])
  (set-location-hidden-features! location (cons hidden-feature (location-hidden-features location))))

(define (remove-hidden-feature-from-location! [location : location] [hidden-feature : Symbol])
  (set-location-hidden-features! location (remove hidden-feature (location-hidden-features location))))

(define (location-has-hidden-feature? [location : location] [hidden-feature : Symbol])
  (memq hidden-feature (location-hidden-features location)))

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

