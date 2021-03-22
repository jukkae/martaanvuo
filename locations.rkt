#lang racket

(require racket/struct)
(require "actions.rkt")
(require "items.rkt")
(require "utils.rkt")

(define-struct location
  (id
   neighbors
   type
   features
   [actors #:mutable]
   items
   tags)
  #:transparent
  #:constructor-name location*

  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'location)
      (lambda (obj)
        (list (unquoted-printing-string "id: ")
              (location-id obj)
              (unquoted-printing-string "neighbors: ")
              (location-neighbors obj)
              (unquoted-printing-string "type: ")
              (location-type obj)
              (unquoted-printing-string "features: ")
              (location-features obj)
              (unquoted-printing-string "actors: ")
              (location-actors obj)
              (unquoted-printing-string "items: ")
              (location-items obj)
              (unquoted-printing-string "tags: ")
              (location-tags obj)
              ))))])

(define (make-location
         #:id id
         #:neighbors neighbors
         #:type type
         #:features features
         #:actors actors
         #:items items
         #:tags tags)
  (location* id neighbors type features actors items tags))

(define (add-actor-to-location! location actor)
  (set-location-actors! location (cons actor (location-actors location))))

(define (remove-actor-from-location! location actor)
  (set-location-actors! location (remove actor (location-actors location))))

(provide (all-defined-out))
