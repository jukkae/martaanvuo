#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "action.rkt")
(require "actor.rkt")
(require "utils.rkt")

(lazy-require
 ["martaanvuo.rkt"
  (engine-function
   pc
   paragraph
   set-in-combat?!
   move-actor-to-location!
   current-location
   stance
   *enemy-stances*)])

(define (make-blindscraper-action actor action-flag)
  (case action-flag

    ['attack
     (define damage-roll (λ () (d 1 2)))
     (define details
       (list
        (cons 'damage-roll damage-roll)
        (cons 'damage-roll-formula "1d2")
        ))
     (make-action
      #:symbol 'melee
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details details)]

    ['go-to-engaged
     (make-action
      #:symbol 'go-to-engaged
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details '())]

    ['blindscrape
     (make-action
      #:symbol 'inflict-status
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details '(blind))]

    [else
     (error (string-append
             "make-blindscraper-action: unknown action: "
             (symbol->string action-flag)))]))


(define (spawn-blindscraper-encounter!)
  (paragraph "A many-jointed fingerlike appendage, long as a forearm, extends from behind a tree trunk. At the tip of the thin finger is a curving shiny black claw. The first finger is followed by several more, then a sac-like, limply hanging body.")

  (set-in-combat?! #t)

  (define i 0)
  (define enemy (make-actor "Blindscraper" 3))
  (set-actor-dexterity! enemy 13)
  (set-trait! enemy "defense" 1)
  (set-trait! enemy "melee-attack-skill" 1)
  (set-trait! enemy "size" "small")
  (move-actor-to-location! enemy (current-location))
  (define index
    (case i
      [(0) "α"]
      [(1) "β"]))
  (define range
    (if (= i 0)
        'close
        'mid))
  (define location
    (case i
      [(0) "right"]
      [(1) "left"]))
  (define enemy-stance
    (stance index range location))
           
  (hash-set! *enemy-stances* enemy enemy-stance))