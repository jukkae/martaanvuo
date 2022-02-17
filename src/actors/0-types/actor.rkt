#lang at-exp typed/racket

(provide (struct-out actor))

(require "../../core/maybe.rkt")

(require "status.rkt"
         "condition.rkt")

(require "../../items/0-types/item.rkt")

(struct actor
  ([name : String]
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

   [inventory : (Listof item)]
   [location : (Maybe Any)] ; TODO: (Maybe location) -> requires typing locations
   [stance : (Maybe Any)]) ; only NPCs ; TODO: (Maybe stance) -> requires typing stances
  #:constructor-name actor*
  #:mutable
  #:prefab)
