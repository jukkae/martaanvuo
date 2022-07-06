#lang at-exp racket

(provide (all-defined-out))

(require
  ; TODO: globber these!
  "blindscraper.rkt"
  "grabberkin.rkt"
  "human-fighter.rkt"
  "voidfloater.rkt"

  "../actors/actor.rkt"
  "../world/world.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"
  "../../3-types/actor.rkt"
  "../../6-combat/combat.rkt"
  "../../6-combat/stance.rkt"
  "../../7-state/state.rkt"
  )

(define (grabberkin-spawn-text)
  (define times-encountered (hash-ref (current-times-species-encountered) 'grabberkin 0))

  (case times-encountered
    [(0) "Something grabs Otava by the ankle and pulls. She staggers, barely staying upright."]
    [(1) "Rotting fingers coil around Otava's ankle with terrible might."]
    [(2) "A Grabberkin grips Otava by the shin and tries to bring her to the ground, jealous of what it can't have."]
    [else "A Grabberkin seizes Otava by the foot."]))

(define (human-fighter-spawn-text)
  (define times-encountered (hash-ref (current-times-species-encountered) 'human-fighter 0))

  (case times-encountered
    [(0) "A man wearing a tracksuit appears, wielding a heavy plumber's wrench."]
    [else "A man wearing a jumpsuit appears, holding a heavy crowbar. There's blood on the jumpsuit."]))

(define (blindscraper-spawn-text)
  (define times-encountered (hash-ref (current-times-species-encountered) 'blindscraper 0))

  (case times-encountered
    [(0) "A many-jointed fingerlike appendage, long as a forearm, extends and folds through the shadows. At the tip of the slender limb is a curving shiny black claw. The first finger is followed by several more, then a sac-like, limply hanging body."]
    [(1) "A gleam of light from the shadows catches Otava's eye. It's another Blindscraper."]
    [else "A Blindscraper crawls to view, silently prowling through the shadows."]))

(define (make-enemy type)
  (case type
    ['grabberkin (make-grabberkin)]
    ['blindscraper (make-blindscraper)]
    ['human-fighter (make-human-fighter)]
    ['voidfloater (make-voidfloater)]
    [else (dev-note (format "Unknown enemy type: ~a" type))]))

(define (spawn type number)
  (for ([i (in-range 0 number)])
    (define sign
      (if (= number 1)
          ""
          (case i
            [(0) "α"]
            [(1) "β"]
            [(2) "γ"]
            [(3) "δ"]
            [(4) "ε"]
            [else (number->string (add1 i))])))

    (define enemy (make-enemy type))

    ; TODO: content
    (case type
      ['blindscraper
        (define range
          (if (= i 0)
              'close
              'mid))
        (define description
          (case i
            [(0) "right"]
            [(1) "left"]
            [else "right"]))
        (define enemy-stance
          (stance sign range description))
        (set-actor-stance! enemy enemy-stance)
       '()]

      ['grabberkin
        (define range 'close) ; can't be grappled with, can be shot with long-barreled guns
        (define description "grabbing Otava's ankle")
        (define enemy-stance
          (stance sign range description))
        (set-actor-stance! enemy enemy-stance)
       '()]

      ['human-fighter
        (define range 'close) ; can't be grappled with, can be shot with long-barreled guns
        (define description "")
        (define enemy-stance
          (stance sign range description))
        (set-actor-stance! enemy enemy-stance)
       '()]

      ['voidfloater
        (define range 'close) ; can't be grappled with, can be shot with long-barreled guns
        (define description "")
        (define enemy-stance
          (stance sign range description))
        (set-actor-stance! enemy enemy-stance)
       '()]

      [else (dev-note (format "unknown enemy type: ~a" type))])

    (move-actor-to-location! enemy (current-location))


    (current-times-species-encountered++ type)
  )
  '())

; TODO: move these to content
(define (spawn-human-fighter-encounter!)
  (p (human-fighter-spawn-text))

  (begin-combat!)

  (spawn 'human-fighter 1))

(define (spawn-voidfloater-encounter!)

  (begin-combat!)
  (spawn 'voidfloater 1))

(define (spawn-grabberkin-encounter!)
  ; could cause fall-down on failed roll

  (p (grabberkin-spawn-text))

  (begin-combat!)
  (spawn 'grabberkin 1)

  (inflict-status! (pc) 'bound (d 2 2)))

(define (spawn-blindscraper-encounter!)
  (p (blindscraper-spawn-text))

  (begin-combat!)

  (spawn 'blindscraper 1))

(define (spawn-two-blindscrapers-encounter!)
  (p "Two blindscrapers appear.")

  (begin-combat!)

  (spawn 'blindscraper 2))

(define (spawn-grabberkin-and-blindscraper-encounter!)
  ; grabberkin
  (begin-combat!)

  (p (grabberkin-spawn-text))
  (p "Otava briefly looks at her restrainted lower appendage, then notices movement in the corner of her eye. Long, fingerlike limbs of a Blindscraper approach silently, folding and bending through the shadows, then its pus-filled drooping sac comes to view.")

  (spawn 'grabberkin 1)
  (inflict-status! (pc) 'bound (d 1 4))

  (spawn 'blindscraper 1)
  )
