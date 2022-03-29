#lang at-exp typed/racket

(provide (struct-out pc-actor))

(require "../2-core/maybe.rkt")

(require "actor.rkt")

(struct pc-actor
 actor
 ([lp : Natural]
  [max-lp : Natural]
  [death-roll-dice : Natural]
  [alive? : Boolean]
  [cause-of-death : (Maybe Symbol)]
  [xp : Natural]
  [hunger : Integer])
 #:constructor-name pc-actor*
 #:prefab
 #:mutable)
