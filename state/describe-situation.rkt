#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "../actor.rkt")
(require "../io.rkt")
(require "../location.rkt")
(require "../pc.rkt")
(require "../place.rkt")
(require "../quest.rkt")
(require "../stance.rkt")
(require "../utils.rkt")

(require "logging.rkt")

(lazy-require
 ["state.rkt" (current-flags
               current-fragment-id
               current-in-combat?
               current-life
               current-location
               current-pc
               current-quests)])

(lazy-require
 ["combat.rkt" (describe-combat-situation)])

(define (describe-non-combat-situation)
  (cond ((null? (current-fragment-id))
         (cond ((eq? (location-id (current-location)) 'perimeter)
                (set-prompt! "Either a climb up the rocky slope to the magpie, or follow the ants to the swamp."))
               ((eq? (location-id (current-location)) 'magpie-hill)
                (p "Natural rock stairs lead back to Perimeter. There's a decrepit industrial building further ahead on the plateau in the fog. There's also a small trail that seems to lead down, towards Martaanvuo swamp.")))
         (cond ((location-has-feature? (current-location) 'magpie-effigy)
                (p "\"Chk-chk\", the magpie calls insistently from the foliage of the skeletonlike forest on the plateau."))))))



(define (describe-situation)
  (when (location-has-feature? (current-location) 'locked-door)
    (cond ((and (pc-has-item? 'revolver)
                (pc-has-ammo-left?))
           (p "There's a door that's locked with a heavy padlock."))
          ((and (pc-has-item? 'bolt-cutters))
           (p "There's a door that's locked with a heavy padlock."))
          (else
           (p "There's a door that's locked with a heavy padlock. If only she had bolt cutters, or something."))))
  (cond
    ((current-in-combat?) (describe-combat-situation))
    (else (describe-non-combat-situation)))
  )

(define (redescribe-situation)
  (cond
    ((current-in-combat?) (describe-combat-situation))
    (else (repeat-last-paragraph)))
  )