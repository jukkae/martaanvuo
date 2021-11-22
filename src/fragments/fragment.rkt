#lang racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 story-fragment
 (id
  on-begin-round!
  content
  decisions))

; NOTE: Fragments are not serialized themselves, only the current index is. This means that any possible fragment state will have to be stored elsewhere. (or s11n implemented somehow)

(define *story-fragments* (make-hash))

(define (fragment
         id
         on-begin-round!
         #:content [content '()]
         #:decisions [decisions '()]
         )
  (define frag
    (story-fragment
     id
     on-begin-round!
     content
     decisions
     ))
  (hash-set! *story-fragments* id frag))

; this can currently error with
; hash-ref: no value found for key
; - if this happens during file load, then the cause is often a stale save file!
(define (get-fragment id)
  (hash-ref *story-fragments* id #;(λ () '())))