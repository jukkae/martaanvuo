#lang at-exp typed/racket

(provide (all-defined-out))

(require
  "../2-core/maybe.rkt"
  "../2-core/list-utils.rkt"

  "sense-organ.rkt"
  )

(struct Clue
  (
   [requires : (Maybe SenseOrgan)]
   [description : String]
   [resolved : Boolean]
   )
   #:constructor-name Clue*
   #:mutable
   #:prefab
  )
