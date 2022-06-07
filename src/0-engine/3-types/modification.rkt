#lang typed/racket

(provide (struct-out Modification))

(require "../2-core/maybe.rkt")

(struct Modification
  ([id : Symbol]
   [name : String]
   [details : (Maybe (U Number Symbol (Listof Symbol)))])
  #:prefab
  #:mutable
  #:constructor-name Modification*)
