#lang at-exp typed/racket

(provide (all-defined-out))

(require "decision.rkt"

         "../../1-index/state.rkt"
         "../../2-core/maybe.rkt"
         "../../2-core/core.rkt"
         "../../2-core/io.rkt")

(require/typed "../../2-core/list-utils.rkt"
               [append-element (∀ (A) (-> (Listof A) A (Listof A)))])

(require/typed "../../2-core/io.rkt"
               [wait-for-input (-> String)]
               [p (-> String)])

(struct story-fragment
  ([id : Symbol]
   [on-begin-round! : (-> (U Null Void))] ; Actually, shouldn't return anything
   [time-taken-by-fragment : Natural]
   [content : String]
   [decisions : (Listof decision)]))

; NOTE: Fragments are not serialized themselves, only the current index is. This means that any possible fragment state will have to be stored elsewhere. (or s11n implemented somehow)

(: *story-fragments* (HashTable Symbol story-fragment))
(define *story-fragments* (make-hash))

(define (fragment
         [id : Symbol]
         [on-begin-round! : (-> (U Null Void)) (λ () '())]
         #:time-taken-by-fragment [time-taken-by-fragment : Natural 0]
         #:content [content : String ""]
         #:decisions [decisions : (Listof decision) '()]
         )
  (define frag
    (story-fragment
     id
     on-begin-round!
     time-taken-by-fragment
     content
     decisions
     ))
  (hash-set! *story-fragments* id frag))

; this can currently error with
; hash-ref: no value found for key
; - if this happens during file load, then the cause is often a stale save file!
(: get-fragment (-> Symbol (Maybe story-fragment)))
(define (get-fragment id)
  (hash-ref *story-fragments* id (λ () '())))
