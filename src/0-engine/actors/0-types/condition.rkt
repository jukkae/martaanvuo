#lang at-exp typed/racket

(provide (struct-out condition))

(require "../../core/maybe.rkt")

(struct condition
 ([type : Symbol]
  [details : (U String (Listof (U Symbol String Number)))])
  #:prefab
  #:mutable)

;; Conditions are semi-permanent.