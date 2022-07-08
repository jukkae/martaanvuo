#lang at-exp typed/racket

(provide (all-defined-out))

(struct stance
  ([sign : String]
   [range : Symbol]
   [description : String])
  #:prefab
  #:mutable)
