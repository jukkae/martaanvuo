#lang at-exp racket

(provide (all-defined-out))

(require reprovide/require-transformer/glob-in)

(require
  (glob-in "../enemies/*.rkt"))

(require
  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"

  "../../0-engine/3-types/location.rkt"
  "../../0-engine/3-types/place.rkt"

  "../../0-engine/4-systems/actors/actor.rkt"
  "../../0-engine/4-systems/blurbs/blurbs.rkt"
  "../../0-engine/4-systems/pc/pc.rkt"

  "../../0-engine/6-combat/combat.rkt"

  "../../0-engine/7-state/logging.rkt"
  "../../0-engine/7-state/mutators.rkt"
  "../../0-engine/7-state/state.rkt"
  )

(define (grabberkin-spawn-text)
  (define times-encountered (hash-ref (current-times-species-encountered) 'grabberkin 0))

  (case times-encountered
    [(0) "Something grabs Otava by the ankle and pulls. She staggers, barely staying upright."]
    [(1) "Rotting fingers coil around Otava's ankle with terrible might."]
    [(2) "A Grabberkin grips Otava by the shin and tries to bring her to the ground, jealous of what it can't have."]
    [else "A Grabberkin seizes Otava by the foot."]))

(define (blindscraper-spawn-text)
  (define times-encountered (hash-ref (current-times-species-encountered) 'blindscraper 0))

  (case times-encountered
    [(0) "A many-jointed fingerlike appendage, long as a forearm, extends and folds through the shadows. At the tip of the slender limb is a curving shiny black claw. The first finger is followed by several more, then a sac-like, limply hanging body."]
    [(1) "A gleam of light from the shadows catches Otava's eye. It's another Blindscraper."]
    [else "A Blindscraper crawls to view, silently prowling through the shadows."]))

    (define (spawn-voidfloater-encounter!)

  (begin-combat!)
  (spawn 'voidfloater 1 'close))

(define (spawn-grabberkin-encounter!)
  ; could cause fall-down on failed roll

  ; (p (grabberkin-spawn-text))

  (begin-combat!)
  (spawn 'grabberkin 1 'engaged)

  (inflict-status! (pc) 'bound (d 2 2)))

(define (spawn-blindscraper-encounter!)
  #;(p (blindscraper-spawn-text))

  (begin-combat!)

  (spawn 'blindscraper 1 'nearby))

(define (spawn-two-blindscrapers-encounter!)
  (begin-combat!)

  (spawn 'blindscraper 2))

(define (spawn-grabberkin-and-blindscraper-encounter!)
  ; grabberkin
  (begin-combat!)

  (p (grabberkin-spawn-text))

  (spawn 'grabberkin 1)
  (inflict-status! (pc) 'bound (d 1 4))

  (spawn 'blindscraper 1)
  )

(define (spawn-human-fighter-encounter!)

  (begin-combat!)

  (spawn 'human-fighter 2 'close))
