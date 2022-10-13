#lang at-exp typed/racket

(provide (all-defined-out))

(require
  "../2-core/maybe.rkt"
  "../2-core/list-utils.rkt"

  "sense-organ.rkt"
  )

(struct Clue
  (
   [requires : (Maybe (U SenseOrgan Symbol))]
   [description : String]
   [resolution-rules : (Maybe Sexp)]
   )
   #:constructor-name Clue*
   #:mutable
   #:prefab
  )
