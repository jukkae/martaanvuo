#lang racket

(provide (all-defined-out))

(require "location.rkt"
         "place.rkt"
         "route.rkt")

(struct world
        (places routes
                day
                elapsed-time
                #;[places : (Listof Place)]
                #;[routes : (Listof route)]
                #;[day : Natural]
                #;[elapsed-time : Natural])
  #:prefab
  #:mutable)
