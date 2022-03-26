#lang typed/racket

(provide (all-defined-out))

(require "../../locations/0-types/location.rkt"
         "../../locations/0-types/route.rkt")

(struct world
  ([places : (Listof place)]
   [routes : (Listof route)]
   [day : Natural]
   [elapsed-time : Natural])
  #:prefab
  #:mutable)
