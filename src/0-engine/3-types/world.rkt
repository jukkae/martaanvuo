#lang typed/racket

(provide (all-defined-out))

(require
  "location.rkt"
  "route.rkt"
  )

(struct world
  ([places : (Listof Place)]
   [routes : (Listof route)]
   [day : Natural]
   [elapsed-time : Natural])
  #:prefab
  #:mutable)
