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

(serializable-struct
 location
 ([id #:mutable] ; has to be mutable for serialization reasons
  [routes #:mutable]
  [type #:mutable] ; has to be mutable for serialization reasons
  [features #:mutable]
  [actors #:mutable]
  [visited? #:mutable]
  [items #:mutable]
  [actions-provided #:mutable]
  [tags #:mutable])

 #:constructor-name location*

 #:methods gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (lambda (obj) 'location)
     (lambda (obj)
       (list
        (unquoted-printing-string "id: ")
        (location-id obj)
        (unquoted-printing-string ", ")
        (unquoted-printing-string "type: ")
        (location-type obj)))))])

(define *number-of-locations* 0)

(define (get-numeric-id) *number-of-locations*)

(define (make-location
         #:id [id (get-numeric-id)]
         #:routes [routes '()]
         #:type [type '()]
         #:features [features '()]
         #:actors [actors '()]
         #:items [items '()]
         #:actions-provided [actions-provided '()]
         #:tags [tags '()])
  (set! *number-of-locations* (add1 *number-of-locations*))
  (location* id routes type features actors #f items actions-provided tags))

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

(define (location-has-tag? location tag)
  (memq tag (location-tags location)))

(define (location-has-feature? location feature)
  (memq feature (location-features location)))

; API
(define (location-is? identifier location)
  (cond ((symbol? identifier)
         (eq? (location-id location)
              identifier))
        (else
         (displayln "not symbol"))
        ))

; internal impl. detail
(define (get-location-name-from-location location)
  (define id (location-id location))
  
  (cond ((and id
              (not (number? id)))
         (cond ((eq? id 'magpie-hill) "Magpie Hill")
               ((eq? id 'martaanvuo-swamp) "Martaanvuo Swamp")
               ((eq? id 'perimeter) "Perimeter")
               ((eq? id 'power-plant-ruins) "Ruins")
               (else (symbol->string id))))
        
        (else
         (define type (location-type location))
         (cond ((eq? type 'swamp) "the swamps")
               ((eq? type 'mountains) "the mountains")
               (else (symbol->string type))))))

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
  (define id (location-id location))
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
                          (symbol->string (location-type location))
                          " "))
     (list (string-append " "
                          "items"
                          " ")
           (string-append " "
                          (~v (location-items location))
                          " "))
     (list (string-append " "
                          "features"
                          " ")
           (string-append " "
                          (~v (location-features location))
                          " "))
     ))
  (info-card body "Location"))

