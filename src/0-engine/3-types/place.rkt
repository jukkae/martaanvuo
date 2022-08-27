#lang at-exp typed/racket

(provide (all-defined-out))

(require
  "location-ids.rkt"
  "location.rkt"
  "actor.rkt"
  "item.rkt"

  "../3-types/choice.rkt"
  "../2-core/maybe.rkt"
  "../3-types/light-levels.rkt"
  "../4-systems/actors/actor.rkt"
  )

(struct Place
  location
  ([routes : (Listof RouteId)]
   [visited? : Boolean]
   [choices : (Listof choice)]
   [shortname : String])

  #:constructor-name Place*
  #:mutable
  #:prefab)
