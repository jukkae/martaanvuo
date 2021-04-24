#lang racket

(require racket/struct)
(require racket/serialize)

(require "action.rkt")
(require "utils.rkt")

(serializable-struct location
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
        (list
         (unquoted-printing-string "\n")
         (unquoted-printing-string "id: ")
         (location-id obj)

         (unquoted-printing-string "\n")
         (unquoted-printing-string "number of neighbors: ")
         #;(location-neighbors obj)
         (length (location-neighbors obj))

         (unquoted-printing-string "\n")
         (unquoted-printing-string "type: ")
         (location-type obj)

         (unquoted-printing-string "\n")
         (unquoted-printing-string "features: ")
         (location-features obj)

         (unquoted-printing-string "\n")
         (unquoted-printing-string "actors: ")
         (location-actors obj)

         (unquoted-printing-string "\n")
         (unquoted-printing-string "visited: ")
         (location-visited obj)

         (unquoted-printing-string "\n")
         (unquoted-printing-string "items: ")
         (location-items obj)

         (unquoted-printing-string "\n")
         (unquoted-printing-string "tags: ")
         (location-tags obj)

         (unquoted-printing-string "\n")
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

(provide (all-defined-out))
