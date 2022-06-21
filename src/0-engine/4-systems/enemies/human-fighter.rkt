#lang at-exp racket

(provide (all-defined-out))

(require
  "../actors/actor.rkt"
  "../checks/checks.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"
  "../../3-types/action.rkt"
  "../../3-types/actor.rkt"
  "../../6-combat/stance.rkt"
  "../../7-state/state.rkt"
  )

(define (make-human-fighter)
  (define enemy (make-actor "Human fighter" 'human-fighter 3))
  (set-actor-dexterity! enemy 13)
  (set-trait! enemy "defense" 1)
  (set-trait! enemy "melee-attack-skill" 1)
  (set-trait! enemy "size" "human")
  enemy)

(define (make-human-fighter-action actor action-flag)
  '())

(define (get-human-fighter-action actor)
  '())

(define (get-human-fighter-reaction actor)
  '())
