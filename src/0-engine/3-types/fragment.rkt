#lang at-exp typed/racket

(provide (all-defined-out))

(require "decision.rkt"

         "../2-core/maybe.rkt")

(define-type FragmentDescription (U String Null))

(struct story-fragment
        ([id : Symbol] [description : FragmentDescription]
                       [on-before-describe! : (-> Null)]
                       [on-after-describe! : (-> Null)]
                       [time-taken-by-fragment : Natural]
                       [decisions : (Listof decision)]))

; NOTE: Fragments are not serialized themselves, only the current index is. This means that any possible fragment state will have to be stored elsewhere. (or s11n implemented somehow)

(: *story-fragments* (HashTable Symbol story-fragment))
(define *story-fragments* (make-hash))

(define (fragment [id : Symbol]
                  [description : FragmentDescription]
                  #:on-before-describe! [on-before-describe! : (-> Null) (λ () '())]
                  #:on-after-describe! [on-after-describe! : (-> Null) (λ () '())]
                  #:time-taken-by-fragment [time-taken-by-fragment : Natural 0]
                  #:decisions [decisions : (Listof decision) '()])
  (define frag
    (story-fragment id
                    description
                    on-before-describe!
                    on-after-describe!
                    time-taken-by-fragment
                    decisions))
  (hash-set! *story-fragments* id frag))

; this can currently error with
; hash-ref: no value found for key
; - if this happens during file load, then the cause is often a stale save file!
(: get-fragment (-> Symbol (Maybe story-fragment)))
(define (get-fragment id)
  (hash-ref *story-fragments* id (λ () '())))
