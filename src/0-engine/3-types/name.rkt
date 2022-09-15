#lang typed/racket

(provide (struct-out Name))

(struct Name
  ([singular : String]
   [indefinite-article : String]
   [possessive-suffix : String]
   [plural : String])
  #:prefab
  #:mutable)
