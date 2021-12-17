#lang racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 actor
 (name

  ; always numbers
  [hp #:mutable]
  [max-hp #:mutable]

  ; number or '()
  [strength #:mutable]
  [dexterity #:mutable]
  [constitution #:mutable]
  [intelligence #:mutable]
  [charisma #:mutable]

  ; hash of string-to-whatever-makes-sense
  [traits #:mutable]

  ; lists of symbols
  [statuses #:mutable]   ; (semi)temporary
  [conditions #:mutable] ; (semi)permanent

  [inventory #:mutable]
  [location #:mutable]
  [stance #:mutable]) ; only NPCs
 #:constructor-name actor*)