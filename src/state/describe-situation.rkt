#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "logging.rkt"

  "../core/io.rkt"
  "../core/utils.rkt"

  "../locations/0-types/location.rkt"

  "../pc/pc.rkt")

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
         (cond ((eq? (location-id (current-location)) 'magpie-hill)
                (p "Natural rock stairs lead back to Perimeter. There's a decrepit industrial building further ahead on the plateau in the fog. A small trail leads along the edge of the plateau.")))
         (cond ((location-has-feature? (current-location) 'magpie-effigy)
                (p "\"Chk-chk\", the magpie calls insistently from the foliage of the skeletonlike forest on the plateau.")))))

  (case (location-id (current-location))
    ['perimeter
     (p @~a{
        A magpie calls from high up the rocky hill on the left. A natural staircase leads up.

        A small squad of ants is marching down the narrow, barely noticeable right-hand trail that's sloping down towards Martaanvuo swamp.

        The air is not right here, it's like she draws it in but it isn't *enough*, like there's too much filth and rottenness and something wet and dirty and heavy in it. Otava's chest feels tight.
      })]))



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
    (else (describe-non-combat-situation))))

(define (redescribe-situation)
  (cond
    ((current-in-combat?) (describe-combat-situation))
    (else (repeat-last-paragraph))))
