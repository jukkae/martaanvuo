#lang racket

(provide (all-defined-out))

(require racket/struct)
(require racket/serialize)

(require "actor.rkt")
(require "action.rkt")
(require "io.rkt") ; TODO: this is only needed for the info card thingy, which likely belongs somewhere else
(require "utils.rkt")

(serializable-struct
 location
 (id
  [neighbors #:mutable]
  type
  features
  [actors #:mutable]
  [visited #:mutable]
  [items #:mutable]
  [actions-provided #:mutable]
  tags)

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

(define (make-location
         #:neighbors [neighbors '()]
         #:type [type '()]
         #:features [features '()]
         #:actors [actors '()]
         #:items [items '()]
         #:actions-provided [actions-provided '()]
         #:tags [tags '()])
  (set! *number-of-locations* (add1 *number-of-locations*))
  (location* *number-of-locations* neighbors type features actors #f items actions-provided tags))

(define (add-actor-to-location! location actor)
  (set-location-actors! location (cons actor (location-actors location))))

(define (remove-actor-from-location! location actor)
  (set-location-actors! location (remove actor (location-actors location))))

(define (add-item-to-location! location item)
  (set-location-items! location (cons item (location-items location))))

(define (remove-item-from-location! location item)
  (set-location-items! location (remove item (location-items location))))

(define (location-has-tag? location tag)
  (memq tag (location-tags location)))

(define (get-location-name-from-location-type location-type)
  (cond ((eq? location-type 'swamp) "the Swamps")
        (else (string-append "get-location-name-from-location-type: unknown location type: " (symbol->string location-type)))))

(define (get-go-to-text-from-location-to-another from-type to-type)
  (case from-type
    ['ridges
     (case to-type
       ['ruins "Climb the hill to the Ruins."]
       ['swamp "Descend to the Swamps."]
       ['edgeflats "Go back to Edgeflats."]
       [else (string-append "Go to " (symbol->string to-type) ".")])]

    ['valleys
     (case to-type
       ['ruins "Climb the hill to the Ruins."]
       ['swamp "Go to the Swamps."]
       ['edgeflats "Go back to Edgeflats."]
       [else (string-append "Go to " (symbol->string to-type) ".")])]

    [else
     (case to-type
       ['ruins "Go to the Ruins."]
       ['swamp "Go to the Swamps."]
       ['edgeflats "Go back to Edgeflats."]
       [else (string-append "Go to " (symbol->string to-type) ".")])
     ]
    )
  )

; TODO: Where does this belong?
(define (display-location-info-card location)
  (define body
    (list
     (list (string-append " "
                          "id"
                          " ")
           (string-append " "
                          (number->string (location-id location))
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
     ))
  (info-card body "Location"))

