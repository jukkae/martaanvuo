#lang typed/racket

(provide (struct-out item)
         (struct-out ranged-weapon))

(require "../2-core/maybe.rkt")

(struct item
  ([id : Symbol]
   [name : String]
   [details : (Maybe (U Number Symbol (Listof Symbol)))]
   [quantity : (U Positive-Integer Positive-Float)])
  #:prefab
  #:mutable
  #:constructor-name item*)

(struct ranged-weapon
 item
 ([ammo-left : Natural])
 #:prefab
 #:mutable
 #:constructor-name ranged-weapon*)
