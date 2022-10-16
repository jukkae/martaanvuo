#lang at-exp typed/racket

(provide (all-defined-out))

(require
  "item.rkt"

  "../2-core/maybe.rkt"
  "../3-types/clue.rkt"
  )


(struct Zone
  ([interactibles : (Listof (U Symbol item))]
   [found? : Boolean]
   [clue? : (Maybe Clue)]
   [pc-here? : Boolean]
   )
  #:prefab
  #:mutable
  #:constructor-name Zone*
  )
