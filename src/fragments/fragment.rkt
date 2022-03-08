#lang at-exp typed/racket

(provide (all-defined-out))

(require "decision.rkt")

(struct
 story-fragment
 ([id : Symbol]
  [on-begin-round! : (-> Null)] ; Actually, shouldn't return anything
  [content : String]
  [decisions : (Listof decision)]))

; NOTE: Fragments are not serialized themselves, only the current index is. This means that any possible fragment state will have to be stored elsewhere. (or s11n implemented somehow)

(: *story-fragments* (HashTable Symbol story-fragment))
(define *story-fragments* (make-hash))

(define (fragment
         [id : Symbol]
         [on-begin-round! : (-> Null) (λ () '())]
         #:content [content : String ""]
         #:decisions [decisions : (Listof decision) '()]
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
(: get-fragment (-> Symbol story-fragment))
(define (get-fragment id)
  (hash-ref *story-fragments* id #;(λ () '())))
