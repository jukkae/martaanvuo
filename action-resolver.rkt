#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(lazy-require
 ["martaanvuo.rkt"
  (award-xp!
   )])

(require "action.rkt")
(require "actor.rkt")
(require "checks.rkt")
(require "info-card.rkt")
(require "situation.rkt")

(define (resolve-melee-action! action)
  (define actor (action-actor action))
  (define target (action-target action))
  
  (define target-defense (get-trait target "defense"))

  (define skill (get-trait actor "melee-attack-skill"))

  (define bonus 0)
  
  (cond ((member 'fallen (actor-statuses target))
         (displayln "[Target fallen, TN -2]")
         (set! bonus 2)
         ))
  (cond ((engaged?)
         (displayln "[Engaged, TN +1]")
         (set! bonus -1)
         ))
  
  (define action-target-number (- 7 bonus))

  (define title
    (string-append "Melee, "
                   (get-combatant-name actor)
                   " vs "
                   (get-combatant-name target)))
  (define success? (skill-check title skill action-target-number))

  (define details (action-details action))
  

  (define damage-roll (assoc 'damage-roll details))
  (define damage-roll-formula (cdr (assoc 'damage-roll-formula details)))
  (define damage-roll-result ((cdr damage-roll)))
  

  (when success?
    (info-card
     (list
      (list " damage roll formula " " result ")
      (list
       (string-append " "
                      damage-roll-formula
                      " ")
       (string-append " "
                      (number->string damage-roll-result)
                      " ")))
     "HP damage roll"))

  (define action-result 'ok)
  (when success? (set! action-result (take-damage target damage-roll-result)))
  (when (eq? action-result 'dead)
    
    ; TODO what's a smart place to store this? the actor?
    (case (actor-name (action-target action))
      [("Blindscraper") (award-xp! 7)]))

  (display-combatant-info target)
  (newline)

  action-result
  )

(define (resolve-wrestle-action! action)
  (define actor (action-actor action))
  (define target (action-target action))
  
  #;(define target-defense (get-trait target "defense"))
  (define target-defense (actor-strength target))

  (define skill (get-trait actor "wrestle-attack-skill"))

  (define bonus 0)
  
  (cond ((member 'fallen (actor-statuses target))
         (displayln "[Target fallen, TN -2]")
         (set! bonus 2)
         ))
  
  (define action-target-number (- 7 bonus))

  (define title
    (string-append "Brawl, "
                   (get-combatant-name actor)
                   " vs "
                   (get-combatant-name target)))
  (define success? (skill-check title skill action-target-number))

  (define details (action-details action))
  

  (define damage-roll (assoc 'damage-roll details))
  (define damage-roll-formula (cdr (assoc 'damage-roll-formula details)))
  (define damage-roll-result ((cdr damage-roll)))
  

  (when success?
    (info-card
     (list
      (list " damage roll formula " " result ")
      (list
       (string-append " "
                      damage-roll-formula
                      " ")
       (string-append " "
                      (number->string damage-roll-result)
                      " ")))
     "HP damage roll"))

  (define action-result 'ok)
  (when success? (set! action-result (take-damage target damage-roll-result)))
  (when (eq? action-result 'dead)
    
    ; TODO what's a smart place to store this? the actor?
    (case (actor-name (action-target action))
      [("Blindscraper") (award-xp! 7)]))

  (display-combatant-info target)
  (newline)

  action-result
  )