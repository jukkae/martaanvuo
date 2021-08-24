#lang racket

(provide (all-defined-out))

(require racket/serialize)

(require "state/combat.rkt"
         "state/state.rkt")

(require "action.rkt")
(require "actor.rkt")
(require "io.rkt")
(require "state/state.rkt")
(require "stance.rkt")
(require "utils.rkt")
(require "world.rkt")

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

    ['go-to-close
     (make-action
      #:symbol 'go-to-close
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

(define (get-blindscraper-action actor)
  (cond ((in-combat?)
         (cond
           ((> (actor-hp actor) 1)

            (cond
              ((actor-in-range? actor 'engaged)
               (define options
                 (list
                  (cons 1 'blindscrape)
                  (cons 1 'blindscrape)
                  (cons 1 'blindscrape)
                  (cons 1 'blindscrape)
                  #;(cons 2 'attack)
                  #;(cons 3 'attack)
                  #;(cons 4 'attack)))
               (define roll (d 1 4))
               (define index (- roll 1))
               #;(displayln "Action")
               (define action-flag-with-index (list-ref options index))
               #;(displayln action-flag-with-index)
               (define action-flag (cdr action-flag-with-index))
               (make-blindscraper-action actor action-flag))
                      
              ((actor-in-range? actor 'close)
               (define options
                 (list
                  (cons 1 'attack)
                  (cons 2 'attack)
                  (cons 3 'go-to-engaged)
                  (cons 4 'go-to-engaged)
                  #;(cons 4 'parry)
                  ))
               (define roll (d 1 4))
               (define index (- roll 1))
               #;(displayln "Action")
               (define action-flag-with-index (list-ref options index))
               #;(displayln action-flag-with-index)
               (define action-flag (cdr action-flag-with-index))
               (make-blindscraper-action actor action-flag))

              (else
               (define options
                 (list
                  (cons 1 'attack)
                  (cons 2 'attack)
                  (cons 3 'go-to-engaged)
                  (cons 4 'go-to-engaged)
                  #;(cons 4 'parry)
                  ))
               (define roll (d 1 4))
               (define index (- roll 1))
               #;(displayln "Action")
               (define action-flag-with-index (list-ref options index))
               #;(displayln action-flag-with-index)
               (define action-flag 'go-to-close)
               (make-blindscraper-action actor action-flag))))
           
           ((= (actor-hp actor) 1)
            (make-action
             #:symbol 'flee
             #:actor actor
             #:duration 1
             #:target '()
             #:tags '(initiative-based-resolution fast)
             #:details '()))))
        (else
         (begin (displayln "Blindscraper AI, not in combat")))))


(define (spawn-blindscraper-encounter!)
  (p "A many-jointed fingerlike appendage, long as a forearm, extends from behind a tree trunk. At the tip of the thin finger is a curving shiny black claw. The first finger is followed by several more, then a sac-like, limply hanging body.")

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
           
  (set-actor-stance! enemy enemy-stance))