#lang typed/racket

(provide (struct-out item)
         (struct-out ranged-weapon)
         )

(require
  "name.rkt"
  "../2-core/maybe.rkt"
  )

(struct item
  ([id : Symbol] ; this is more of a "title" currently – TODO: add an unique id to be used for searching and comparisons
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
