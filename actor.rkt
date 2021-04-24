#lang racket

(provide (all-defined-out))

(require racket/serialize)

(serializable-struct
 actor
 (name
  [hp #:mutable]
  max-hp
  attack-skill
  attack-damage
  defense-number
  dexterity
  inventory
  statuses
  conditions
  [current-location #:mutable]))

(serializable-struct
 pc-actor
 (lp
  max-lp)
 #:super struct:actor)