#lang typed/racket

(provide (struct-out item)
         (struct-out ranged-weapon)
         (struct-out Name)
         )

(require "../2-core/maybe.rkt")

(struct Name
  ([singular : String]
   [indefinite-article : String]
   [possessive-suffix : String]
   [plural : String])
  #:prefab
  #:mutable)

(struct item
  ([id : Symbol]
   [name : (U String Name)]
   [details : (Maybe (U Number Symbol (Listof Symbol)))]
   [quantity : (U Nonnegative-Integer Nonnegative-Float)])
  #:prefab
  #:mutable
  #:constructor-name item*)

(struct ranged-weapon
 item
 ([ammo-left : Natural])
 #:prefab
 #:mutable
 #:constructor-name ranged-weapon*)
