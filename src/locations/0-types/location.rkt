#lang racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 location
 ([id #:mutable] ; mutable for s11n, but should not be mutated
  [type #:mutable]
  [details #:mutable]
  [actors #:mutable]
  [items #:mutable]
  [features #:mutable]
  [tags #:mutable])
 #:transparent
 #:constructor-name location*)

(define (add-actor-to-location! location actor)
  (set-location-actors! location (cons actor (location-actors location))))

(define (remove-actor-from-location! location actor)
  (set-location-actors! location (remove actor (location-actors location))))

(define (add-item-to-location! location item)
  (set-location-items! location (cons item (location-items location))))

(define (remove-item-from-location! location item)
  (set-location-items! location (remove item (location-items location))))

(define (add-feature-to-location! location feature)
  (set-location-features! location (cons feature (location-features location))))

(define (remove-feature-from-location! location feature)
  (set-location-features! location (remove feature (location-features location))))

(define (location-has-feature? location feature)
  (memq feature (location-features location)))

(define (add-detail-to-location! location detail)
  (when (not (location-has-detail? location detail))
    (set-location-details! location (cons detail (location-details location)))))

(define (remove-detail-from-location! location detail)
  (when (location-has-detail? location detail)
    (set-location-details! location (remove detail (location-details location)))))

(define (location-has-detail? location detail)
  (memq detail (location-details location)))

(define (location-has-tag? location tag)
  (memq tag (location-tags location)))

; API
(define (location-is? identifier location)
  (eq? (location-id location)
       identifier))
