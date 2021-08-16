#lang racket

(provide (all-defined-out))

(require racket/serialize)

(require "utils.rkt")


(serializable-struct
 story-fragment
 (id
  description
  decisions
  on-enter!
  on-begin-round!))

; NOTE: Fragments are not serialized themselves, only the current index is. This means that any possible fragment state will have to be stored elsewhere.

(define *story-fragments* (make-hash))

(define (fragment
         id
         description
         #:decisions decisions ; initialize to "confirm to exit", or treat '() as such
         #:on-enter! [on-enter! (λ () '())] ; this is more for setting preconditions
         #:on-begin-round! [on-begin-round! (λ () '())])
  (define frag
    (story-fragment
     id
     description
     decisions
     on-enter!
     on-begin-round!))
  (hash-set! *story-fragments* id frag))

(define (get-fragment id)
  (hash-ref *story-fragments* id))