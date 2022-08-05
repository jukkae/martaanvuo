#lang typed/racket

(provide (struct-out SenseOrgan))

(struct SenseOrgan
  ([id : Symbol]
   [name : String])
  #:prefab
  #:mutable)
