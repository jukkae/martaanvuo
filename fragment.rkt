#lang racket

(provide (all-defined-out))

(require racket/serialize)

(require "utils.rkt")


(serializable-struct
 story-fragment
 (id
  on-begin-round!
  decisions
  on-enter!))

; NOTE: Fragments are not serialized themselves, only the current index is. This means that any possible fragment state will have to be stored elsewhere. (or s11n implemented somehow)

(define *story-fragments* (make-hash))

(define (fragment
         id
         on-begin-round!
         #:decisions decisions ; initialize to "confirm to exit", or treat '() as such
         #:on-enter! [on-enter! (λ () '())] ; this is more for setting preconditions - ie., before-on-begin-round!, which should actually not be used
         )
  (define frag
    (story-fragment
     id
     on-begin-round!
     decisions
     on-enter!
     ))
  (hash-set! *story-fragments* id frag))

(define (get-fragment id)
  (hash-ref *story-fragments* id))