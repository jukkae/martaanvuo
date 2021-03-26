#lang racket

(require racket/struct)
(require "actions.rkt")
(require "items.rkt")
(require "utils.rkt")

(define-struct location
  (id
   [neighbors #:mutable]
   type
   features
   [actors #:mutable]
   [visited #:mutable]
   items
   tags)

  #:constructor-name location*

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'location)
      (lambda (obj)
        (list (unquoted-printing-string "id: ")
              (location-id obj)
              (unquoted-printing-string "number of neighbors: ")
              #;(location-neighbors obj)
              (length (location-neighbors obj))
              (unquoted-printing-string "type: ")
              (location-type obj)
              (unquoted-printing-string "features: ")
              (location-features obj)
              (unquoted-printing-string "actors: ")
              (location-actors obj)
              (unquoted-printing-string "visited: ")
              (location-visited obj)
              (unquoted-printing-string "items: ")
              (location-items obj)
              (unquoted-printing-string "tags: ")
              (location-tags obj)
              ))))])

(define *number-of-locations* 0)

(define (make-location
         #:neighbors neighbors
         #:type type
         #:features features
         #:actors actors
         #:items items
         #:tags tags)
  (set! *number-of-locations* (add1 *number-of-locations*))
  (location* *number-of-locations* neighbors type features actors #f items tags))

(define (add-actor-to-location! location actor)
  (set-location-actors! location (cons actor (location-actors location))))

(define (remove-actor-from-location! location actor)
  (set-location-actors! location (remove actor (location-actors location))))

(define (make-go-to-neighbor-choices location)
  (for/list ([neighbor (location-neighbors location)])
    (make-choice 'go-to-neighboring-location
                 (string-append "Go to "
                                (symbol->string (location-type neighbor))
                                " [id: "
                                (number->string (location-id neighbor))
                                "]")
                 (λ () (make-action #:symbol 'go-to-neighboring-location
                                    #:actor 'pc
                                    #:duration 100 ; 100 jiffies - half-a-day -> action economy: get better -> slightly better actions
                                    #:target neighbor
                                    #:tags '(wilderness downtime))))))

(provide (all-defined-out))
