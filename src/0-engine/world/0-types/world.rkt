#lang typed/racket

(provide (all-defined-out))

(require
  "../../3-types/location.rkt"
  "../../3-types/route.rkt"
  )

(struct world
  ([places : (Listof place)]
   [routes : (Listof route)]
   [day : Natural]
   [elapsed-time : Natural])
  #:prefab
  #:mutable)
