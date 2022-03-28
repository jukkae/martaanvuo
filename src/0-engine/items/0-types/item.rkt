#lang typed/racket

(provide (struct-out item)
         (struct-out ranged-weapon))

(require "../../core/maybe.rkt")

(struct item
  ([id : Symbol]
   [name : String]
   [details : (Maybe Number)]
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