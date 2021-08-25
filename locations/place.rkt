#lang racket

(provide (all-defined-out))

(require racket/struct)
(require racket/serialize)

(require "../io.rkt")
(require "../utils.rkt")


(serializable-struct
 place
 ([routes #:mutable]
  [type #:mutable] ; has to be mutable for serialization reasons
  [on-enter-symbol #:mutable] ; symbol, because lambdas cannot be easily serialized
  [visited? #:mutable]
  [actions-provided #:mutable]
  [shortname #:mutable])

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
         #:on-enter-symbol [on-enter-symbol '()]
         #:features [features '()]
         #:actors [actors '()]
         #:items [items '()]
         #:actions-provided [actions-provided '()]
         #:tags [tags '()]
         #:shortname [shortname ""])
  (set! *number-of-places* (add1 *number-of-places*))
  (place* id routes type on-enter-symbol features actors #f items actions-provided tags shortname))


(define (display-place-info-card location [title "Location"])
  (define id (place-id location))
  (define body
    (prune (list
            (when (not (eq? (place-shortname location) ""))
              (list (string-append " "
                                   (place-shortname location)
                                   " ")
                    "  "))
            (when (not (null? (place-id location)))
              (list (string-append " "
                                   "id"
                                   " ")
                    (string-append " "
                                   (cond ((number? id) (number->string id))
                                         ((symbol? id) (symbol->string id)))
                                   " ")))
            (when (and (null? (place-id location))
                       (not (null? (place-type location))))
              (list (string-append " "
                                   "type"
                                   " ")
                    (string-append " "
                                   (symbol->string (place-type location))
                                   " ")))
            (when (not (null? (place-items location)))
              (list (string-append " "
                                   "items"
                                   " ")
                    (string-append " "
                                   (~v (place-items location))
                                   " ")))
            (when (not (null? (place-features location)))
              (list (string-append " "
                                   "features"
                                   " ")
                    (string-append " "
                                   (~v (place-features location))
                                   " ")))
            )))
  (info-card body title))