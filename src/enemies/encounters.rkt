#lang racket

(provide (all-defined-out))

(require
  "blindscraper.rkt"
  "grabberkin.rkt"
  
  "../actions/action.rkt"

  "../actors/actor.rkt" 
  "../actors/stance.rkt"
  "../actors/status.rkt"

  "../core/io.rkt"
  "../core/utils.rkt"

  "../state/combat.rkt"
  "../state/state.rkt"

  "../world/world.rkt")

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

(define (make-enemy type)
  (case type
    ['grabberkin (make-grabberkin)]
    ['blindscraper (make-blindscraper)]
    [else (dev-note (format "Unknown enemy type: ~a" type))]))

(define (spawn-enemies type number)
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

      [else (dev-note (format "unknown enemy type: ~a" type))])

    (move-actor-to-location! enemy (current-location))


    (current-times-species-encountered++ type)
  )
  '())

(define (spawn-grabberkin-encounter!)
  ; could cause fall-down on failed roll

  (p (grabberkin-spawn-text))

  (begin-combat!)
  (spawn-enemies 'grabberkin 1)

  (inflict-status! (pc) 'bound (d 2 3)))

(define (spawn-blindscraper-encounter!)
  (p (blindscraper-spawn-text))

  (begin-combat!)

  (spawn-enemies 'blindscraper 1))

(define (spawn-two-blindscrapers-encounter!)
  (p "Two blindscrapers appear.")

  (begin-combat!)

  (spawn-enemies 'blindscraper 2))

(define (spawn-grabberkin-and-blindscraper-encounter!)
  ; grabberkin
  (begin-combat!)

  (p (grabberkin-spawn-text))
  (p "Otava briefly looks at her restrainted lower appendage, then notices movement in the corner of her eye. Long, fingerlike limbs of a Blindscraper approach silently, folding and bending through the shadows, then its pus-filled drooping sac comes to view.")

  (spawn-enemies 'grabberkin 1)
  (inflict-status! (pc) 'bound (d 2 3))

  (spawn-enemies 'blindscraper 1)
  )