#lang at-exp typed/racket

(provide (all-defined-out))

(require "item.rkt"

         "../2-core/maybe.rkt"
         "../3-types/clue.rkt")

(define-type Feature Symbol)

(struct Zone
        ([interactibles : (Listof (U Feature item))] [found? : Boolean]
                                                     [clue? : (Maybe Clue)]
                                                     [pc-here? : Boolean]
                                                     [description : String]
                                                     [name : String])
  #:prefab
  #:mutable
  #:constructor-name Zone*)
