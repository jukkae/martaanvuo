#lang racket

(provide (all-defined-out))

(require racket/struct)
(require racket/serialize)

(require "location.rkt")


(serializable-struct
 place
 location
 ([routes #:mutable]
  [on-enter-symbol #:mutable] ; symbol, because lambdas cannot be easily serialized
  [visited? #:mutable]
  [actions-provided #:mutable]
  [shortname #:mutable])

 #:constructor-name place*)

(define *number-of-places* 0)

(define (get-numeric-id) *number-of-places*)

(define (make-place
         #:id [id (get-numeric-id)]
         #:type [type '()]
         #:details [details '()]
         #:actors [actors '()]
         #:items [items '()]
         #:features [features '()]
         #:tags [tags '()]
         #:routes [routes '()]
         #:on-enter-symbol [on-enter-symbol '()]
         #:visited? [visited? #f]
         #:actions-provided [actions-provided '()]
         #:shortname [shortname ""])
  
  (set! *number-of-places* (add1 *number-of-places*))
  
  (place* id
          type
          details
          actors
          items
          features
          tags
          routes
          on-enter-symbol
          visited?
          actions-provided
          shortname))