#lang at-exp typed/racket

(provide (struct-out pc-actor))

(require "../2-core/maybe.rkt")

(require "actor.rkt"
         "manipulator.rkt"
         "modification.rkt"
         "sense-organ.rkt")

(struct pc-actor
        actor
        ([lp : Natural] [max-lp : Natural]
                        [death-roll-dice : Natural]
                        [alive? : Boolean]
                        [cause-of-death : (Maybe (U Symbol String))]
                        [xp : Natural]
                        [hunger : Integer]
                        [fatigue : Integer]
                        [modifications : (Listof Modification)]
                        [sense-organs : (Listof SenseOrgan)]
                        [manipulators : (Listof Manipulator)])
  #:constructor-name pc-actor*
  #:prefab
  #:mutable)
