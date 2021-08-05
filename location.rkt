#lang racket

(provide (all-defined-out))

(require racket/struct)
(require racket/serialize)

(require "actor.rkt")
(require "action.rkt")
(require "io.rkt") ; TODO: this is only needed for the info card thingy, which likely belongs somewhere else
(require "place.rkt")
(require "route.rkt")
(require "utils.rkt")

(define (add-actor-to-location! location actor)
  (cond ((route? location)
         (set-route-actors! location (cons actor (route-actors location))))
        ((place? location)
         (set-place-actors! location (cons actor (place-actors location))))))

(define (remove-actor-from-location! location actor)
  (cond ((route? location)
         (set-route-actors! location (cons actor (route-actors location))))
        ((place? location)
         (set-place-actors! location (remove actor (place-actors location)))))
  )

(define (location-actors location)
  (cond ((route? location)
         (route-actors location))
        ((place? location)
         (place-actors location))))

(define (location-id location)
  (cond ((route? location)
         (route-id location))
        ((place? location)
         (place-id location))))

(define (location-actions-provided location)
  (cond ((route? location)
         '())
        ((place? location)
         (place-actions-provided location))))

(define (location-features location)
  (cond ((route? location)
         '())
        ((place? location)
         (place-features location))))

(define (set-location-features! location feature)
  (cond ((place? location)
         (place-features location feature))))

(define (location-type location)
  (cond ((route? location)
         '())
        ((place? location)
         (place-type location))))

(define (location-on-enter-symbol location)
  (cond ((route? location)
         '())
        ((place? location)
         (place-on-enter-symbol location))))

(define (location? it)
  (or (place? it)
      (route? it)))

(define (location-routes location)
  (cond ((route? location)
         '())
        ((place? location)
         (place-routes location))))

; TODO think which of these are really needed
(define (add-item-to-location! location item)
  (set-place-items! location (cons item (place-items location))))

(define (remove-item-from-location! location item)
  (set-place-items! location (remove item (place-items location))))

(define (location-items location)
  (cond ((route? location)
         '())
        ((place? location)
         (place-items location))))

(define (add-feature-to-location! location feature)
  (set-place-features! location (cons feature (place-features location))))

(define (remove-feature-from-location! location feature)
  (set-place-features! location (remove feature (place-features location))))

(define (location-has-tag? location tag)
  (cond ((route? location)
         #f)
        ((place? location)
         (memq tag (place-tags location)))))

(define (location-has-feature? location feature)
  (cond ((route? location)
         #f)
        ((place? location)
         (memq feature (place-features location)))))

; API
(define (location-is? identifier location)
  (cond ((symbol? identifier)
         (cond ((route? location)
                (eq? (route-id location)
                     identifier))
               ((place? location)
                (eq? (place-id location)
                     identifier))))
        (else
         (displayln "location-is?: identifier is not symbol"))
        ))

; internal impl. detail
(define (get-location-name-from-location location)
  (define id
    (cond ((route? location)
           (route-id location))
          ((place? location)
           (place-id location))))
  
  (cond ((and id
              (symbol? id))
         (cond ((eq? id 'magpie-hill) "Magpie Hill")
               ((eq? id 'martaanvuo-swamp) "Martaanvuo Swamp")
               ((eq? id 'perimeter) "Perimeter")
               ((eq? id 'power-plant-ruins) "Ruins")
               (else (symbol->string id))))

        ((number? id)
         "id is number, check get-location-name-from-location")
        ))

; internal
(define (get-location-short-description location)
  (define name (get-location-name-from-location location))
  (define features-str
    ; Disabled for now, just do empty string
    #;(cond ((not (null? (location-features location)))
             (cond ((memq 'magpie-effigy (location-features location))
                    "Magpie Effigy")
                   (else "Unknown features TODO")))
            (else ; no features
             ""))
    "")
  (string-append name
                 features-str)
  )

; TODO: Where does this belong?
(define (display-location-info-card location)
  (define id (place-id location))
  (define body
    (list
     (list (string-append " "
                          "id"
                          " ")
           (string-append " "
                          (cond ((number? id) (number->string id))
                                ((symbol? id) (symbol->string id)))
                          " "))
     (list (string-append " "
                          "type"
                          " ")
           (string-append " "
                          (symbol->string (place-type location))
                          " "))
     (list (string-append " "
                          "items"
                          " ")
           (string-append " "
                          (~v (place-items location))
                          " "))
     (list (string-append " "
                          "features"
                          " ")
           (string-append " "
                          (~v (place-features location))
                          " "))
     ))
  (info-card body "Location"))

