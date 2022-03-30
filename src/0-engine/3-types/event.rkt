#lang typed/racket

(provide (all-defined-out))

(require "../2-core/maybe.rkt")

(struct event
  ([type : Symbol] ; should be enumerated
   [details : (Maybe (U Symbol String))] ; ??
   [interrupting? : Boolean]
   [at : Natural] ; More properly: in-game timestamp
   )
  #:prefab
  #:mutable
  #:constructor-name event*
  )

(define (format-event-for-display (event : event))
  (list
   (number->string (event-at event))
   (symbol->string (event-type event))
   (~s (event-details event))
   (if (event-interrupting? event)
       "yes"
       "no")))