#lang typed/racket

(provide (all-defined-out))

(struct combat-event ([details : String] [at : Natural])
  #:constructor-name combat-event*
  #:prefab
  #:mutable)
