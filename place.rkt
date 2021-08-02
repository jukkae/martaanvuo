#lang racket

(provide (all-defined-out))

(require racket/struct)
(require racket/serialize)


(require "io.rkt") ; TODO: this is only needed for the info card thingy, which likely belongs somewhere else
(require "utils.rkt")


(serializable-struct
 place
 ([id #:mutable] ; has to be mutable for serialization reasons
  [routes #:mutable]
  [type #:mutable] ; has to be mutable for serialization reasons
  [features #:mutable]
  [actors #:mutable]
  [visited? #:mutable]
  [items #:mutable]
  [actions-provided #:mutable]
  [tags #:mutable])

 #:constructor-name place*

 #:methods gen:custom-write
 [(define write-proc
    (make-constructor-style-printer
     (lambda (obj) 'place)
     (lambda (obj)
       (list
        (unquoted-printing-string "id: ")
        (place-id obj)
        (unquoted-printing-string ", ")
        (unquoted-printing-string "type: ")
        (place-type obj)))))])

(define *number-of-places* 0)

(define (get-numeric-id) *number-of-places*)

(define (make-place
         #:id [id (get-numeric-id)]
         #:routes [routes '()]
         #:type [type '()]
         #:features [features '()]
         #:actors [actors '()]
         #:items [items '()]
         #:actions-provided [actions-provided '()]
         #:tags [tags '()])
  (set! *number-of-places* (add1 *number-of-places*))
  (place* id routes type features actors #f items actions-provided tags))