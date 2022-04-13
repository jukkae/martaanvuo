#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"

  "../../0-engine/3-types/location.rkt"

  "../../0-engine/4-systems/blurbs/blurbs.rkt"
  "../../0-engine/4-systems/pc/pc.rkt"

  "../../0-engine/7-state/logging.rkt"
  "../../0-engine/7-state/mutators.rkt"
  "../../0-engine/7-state/state.rkt"
  )

(lazy-require ["../../0-engine/6-combat/narration.rkt"
 (describe-combat-situation
  )])

(define (describe-non-combat-situation repeated?)
  (cond ((null? (current-fragment-id))
         (cond ((eq? (location-id (current-location)) 'magpie-hill)
                (p #:suppress-logging? repeated? "Natural rock stairs lead back to Perimeter. There's a decrepit industrial building further ahead on the plateau in the fog. A small trail leads along the edge of the plateau.")))
         (cond ((location-has-feature? (current-location) 'magpie-effigy)
                (p #:suppress-logging? repeated?"\"Chk-chk\", the magpie calls insistently from the foliage of the skeletonlike forest on the plateau.")))))

  ; TODO: extract this to a function
  (case (location-id (current-location))
    ['perimeter
     (p #:suppress-logging? repeated? "A magpie calls from high up the rocky hill on the left. It's steep, but the path continues promisingly.")
     ;(next-blurb 'ants)
     (if (not (place-visited? (current-location)))
      (p #:suppress-logging? repeated? "It takes a while for Otava to notice the other fork under fallen rock. A crew of ants is carrying chopped-up leaves of a hardy plant down the second path.")
      (p #:suppress-logging? repeated? "A second path leads to the right.")
       )

    (if (not (place-visited? (current-location)))
      (p #:suppress-logging? repeated? "The air is not quite right here, it's like she draws it in but it isn't *enough*, like there's too much filth and rottenness and something wet and dirty and heavy in it. Otava's chest feels tight.")
      (p #:suppress-logging? repeated? "The air is filthy and wrong and rotten and heavy. There's a faint smell of rotten flesh in it, a sticky metallic aftertaste in the mouth.")
    )
     (when (not (flag-set? 'tried-to-go-back))
       '())
     ]
    ['burnt-tree
      (p "The vengeful husk of the burnt tree, a vast deep black silhouette against the sky. Near ground, there's a slit in its outer shell, just wide enough for Otava to fit through. Inside, nothing but black and and an intense smell of ash.")]))


(define (describe-situation repeated?)
  (when (location-has-feature? (current-location) 'locked-door)
    (cond ((and (pc-has-item? 'revolver)
                (pc-has-ammo-left?))
           (p "There's a door that's locked with a heavy padlock."))
          ((and (pc-has-item? 'bolt-cutters))
           (p "There's a door that's locked with a heavy padlock."))
          (else
           (p "There's a door that's locked with a heavy padlock. If only she had bolt cutters..."))))
  (cond
    ((current-in-combat?) (describe-combat-situation))
    (else
      (describe-non-combat-situation repeated?))))

