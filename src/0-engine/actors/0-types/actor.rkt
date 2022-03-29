#lang at-exp typed/racket

(provide (struct-out actor)
         ActorId)

(require
  "../../2-core/maybe.rkt"
  "../../3-types/status.rkt"
  "../../3-types/condition.rkt"
  "../../3-types/location-ids.rkt"
  "../../items/0-types/item.rkt"
  "../../combat/stance.rkt"
  )

(define-type ActorId (U Symbol Natural))

(struct actor
  ([id : ActorId]
   [name : String]
   [type : Symbol]
   [hp : Integer]
   [max-hp : Natural]

   [strength : (Maybe Natural)]
   [dexterity : (Maybe Natural)]
   [constitution : (Maybe Natural)]
   [intelligence : (Maybe Natural)]
   [charisma : (Maybe Natural)]

   [traits : (HashTable String (U Symbol Number Boolean String '()))]

   [statuses : (Listof status)]   ; (semi)temporary
   [conditions : (Listof condition)] ; (semi)permanent

   [inventory : (Listof (U item Symbol))]
   [location-id : (Maybe LocationId)]
   [stance : (Maybe stance)]) ; only NPCs
  #:constructor-name actor*
  #:mutable
  #:prefab)
