#lang racket

(require "actions.rkt")
(require "items.rkt")
(require "utils.rkt")

(define-struct location
  (id
   neighbors
   type
   features
   actors
   items
   tags)
  #:constructor-name location*)

(define (make-location
         #:id id
         #:neighbors neighbors
         #:type type
         #:features features
         #:actors actors
         #:items items
         #:tags tags)
  (location* id neighbors type features actors items tags))

(provide (all-defined-out))
