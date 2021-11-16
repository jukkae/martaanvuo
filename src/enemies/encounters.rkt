#lang racket

(provide (all-defined-out))

(require "../state/combat.rkt"
         "../state/state.rkt")

(require "../action.rkt"
         "../actor.rkt" 
         "../io.rkt"
         "../stance.rkt"
         "../status.rkt"
         "../utils.rkt"
         "../world.rkt")

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

(define (spawn-grabberkin-encounter!)
  ; could cause fall-down on failed roll

  (p (grabberkin-spawn-text))

  (begin-combat!)

  (define hp 11)
  (define i 0)
  (define enemy (make-actor "Grabberkin" hp))
  (set-actor-dexterity! enemy 4)
  (set-actor-strength! enemy 11)
  (set-trait! enemy "defense" -1)
  (set-trait! enemy "melee-attack-skill" 1)
  (set-trait! enemy "hp-hidden" #f)
  (move-actor-to-location! enemy (current-location))

  (inflict-status! (pc) (status 'bound 10))

  (define sign
    (case i
      [(0) "α"]
      [(1) "β"]
      [(2) "γ"]
      [(3) "δ"]
      [else ""]))
  (define range 'engaged)
  (define description "grabbing Otava's ankle")
  (define enemy-stance
    (stance sign range description))
           
  (set-actor-stance! enemy enemy-stance)

  (hash-set! (current-times-species-encountered)
             'grabberkin
             (add1 (hash-ref (current-times-species-encountered)
                              'grabberkin
                              0)))
  )

(define (spawn-grabberkin-and-blindscraper-encounter!)
  ; grabberkin
  (begin-combat!)

  (p (grabberkin-spawn-text))
  (p "Otava briefly looks at her restrainted lower appendage, then notices movement in the corner of her eye. Long, fingerlike limbs of a Blindscraper approach silently, folding and bending through the shadows, then its pus-filled drooping sac comes to view.")

  (define hp 11)

  (define e1 (make-actor "Grabberkin" hp))
  (set-actor-dexterity! e1 4)
  (set-actor-strength! e1 11)
  (set-trait! e1 "defense" -1)
  (set-trait! e1 "melee-attack-skill" 1)
  (set-trait! e1 "hp-hidden" #f)
  (move-actor-to-location! e1 (current-location))

  (inflict-status! (pc) (status 'bound 10))
         
  (set-actor-stance! e1 (stance "α" 'engaged "grabbing Otava's ankle"))
  

  (define i 1)
  (define enemy (make-actor "Blindscraper" 3))
  (set-actor-dexterity! enemy 13)
  (set-trait! enemy "defense" 1)
  (set-trait! enemy "melee-attack-skill" 1)
  (set-trait! enemy "size" "small")
  (move-actor-to-location! enemy (current-location))

  (define sign
    (case i
      [(0) "α"]
      [(1) "β"]
      [(2) "γ"]
      [(3) "δ"]
      [else ""]))
  
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
  
  (hash-set! (current-times-species-encountered)
             'grabberkin
             (add1 (hash-ref (current-times-species-encountered)
                              'grabberkin
                              0)))
  )


(define (spawn-blindscraper-encounter!)
  (p (blindscraper-spawn-text))

  (begin-combat!)

  (define i 1)
  (define enemy (make-actor "Blindscraper" 3))
  (set-actor-dexterity! enemy 13)
  (set-trait! enemy "defense" 1)
  (set-trait! enemy "melee-attack-skill" 1)
  (set-trait! enemy "size" "small")
  (move-actor-to-location! enemy (current-location))

  (define sign
    (case i
      [(0) "α"]
      [(1) "β"]
      [(2) "γ"]
      [(3) "δ"]
      [else ""]))
  
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
  
  (hash-set! (current-times-species-encountered)
             'blindscraper
             (add1 (hash-ref (current-times-species-encountered)
                              'blindscraper
                              0)))
  )

(define (spawn-two-blindscrapers-encounter!)
  (p "Two blindscrapers appear.")

  (begin-combat!)

  (for ([i 2])
    (define enemy (make-actor "Blindscraper" 3))
    (set-actor-dexterity! enemy 13)
    (set-trait! enemy "defense" 1)
    (set-trait! enemy "melee-attack-skill" 1)
    (set-trait! enemy "size" "small")
    (move-actor-to-location! enemy (current-location))

    (define sign
      (case i
        [(0) "α"]
        [(1) "β"]
        [(2) "γ"]
        [(3) "δ"]
        [else ""]))
    
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
    )
)