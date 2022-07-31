#lang typed/racket

(provide (struct-out Manipulator))

(struct Manipulator
  ([id : Symbol]
   [name : String])
  #:prefab
  #:mutable)
