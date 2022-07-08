#lang at-exp typed/racket

(provide (struct-out actor)
         ActorId
         get-attribute-modifier-for
         get-modifier-string)

(require
  "../2-core/maybe.rkt"
  "../3-types/condition.rkt"
  "../3-types/item.rkt"
  "../3-types/location-ids.rkt"
  "../3-types/status.rkt"
  "../3-types/stance.rkt"
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

(define (get-attribute-modifier-for [attribute-value : Natural])
  (cond ((= attribute-value 3) -3)
        ((<= 4  attribute-value  5) -2)
        ((<= 6  attribute-value  8) -1)
        ((<= 9  attribute-value 12)  0)
        ((<= 13 attribute-value 15)  1)
        ((<= 16 attribute-value 17)  2)
        ((= attribute-value 18) 3)))

(define (get-modifier-string [modifier : Natural])
  (cond ((negative? modifier) (number->string modifier))
        ((= 0 modifier) (number->string modifier))
        ((positive? modifier) (format "+~a" modifier))))
