#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "action.rkt")
(require "actor.rkt")
(require "pc.rkt")
(require "utils.rkt")
(require "world.rkt")

(lazy-require
 ["martaanvuo.rkt"
  (engine-function
   pc
   paragraph
   in-combat?
   actor-in-range?
   set-in-combat?!
   move-actor-to-location!
   current-location
   stance
   *enemy-stances*)])

(serializable-struct
 situation
 (world
  [pc #:mutable]
  [life #:mutable]
  [run #:mutable]
  [round #:mutable]
  [elapsed-time #:mutable]
  [in-combat? #:mutable]
  [current-fragment #:mutable]
  [quests #:mutable]
  [grabberkin-encounters #:mutable]
  ))

(define *situation*
  (let ([new-world (world (list edgeflats swamp ridges valleys crematory ruins sewers cache workshop spring) 0 0)]
        [pc (make-new-pc)]
        [quests '()])
    (situation new-world pc 0 0 0 0 #f '() quests 0)))