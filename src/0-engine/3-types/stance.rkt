#lang at-exp typed/racket

(provide (all-defined-out))

(define-type StanceRange (U 'engaged 'adjacent 'close 'nearby 'far))
(struct stance
  ([sign : String]
   [range : StanceRange]
   [description : String])
  #:prefab
  #:mutable)
