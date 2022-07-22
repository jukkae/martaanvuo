#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"

  "../../0-engine/3-types/location.rkt"
  "../../0-engine/3-types/place.rkt"

  "../../0-engine/4-systems/blurbs/blurbs.rkt"
  "../../0-engine/4-systems/pc/pc.rkt"

  "../../0-engine/7-state/logging.rkt"
  "../../0-engine/7-state/mutators.rkt"
  "../../0-engine/7-state/state.rkt"
  )

(lazy-require ["../../0-engine/6-combat/narration.rkt"
 (describe-combat-situation
  )])

(define (describe-location repeated?)
(case (location-id (current-location))
    ['perimeter
     (p #:suppress-logging? repeated? "A magpie calls from high up the rocky hill on the left. Another path leads to the right, through low, prickly bushes.")
     (when (not (Place-visited? (current-location))) ; logic broken
      (p #:suppress-logging? repeated? "A crew of ants is carrying chopped-up leaves of a hardy plant down the second path."))

    (if (not (Place-visited? (current-location)))
      (p #:suppress-logging? repeated? "The hot, stale air is not quite right here, it's like she draws it in but it isn't *enough*, like there's too much filth and rottenness and something dirty and heavy in it. Otava's chest feels tight.")
      (p #:suppress-logging? repeated? "The hot air is filthy and wrong and heavy. There's a faint smell of rotten flesh in it, a sticky metallic aftertaste in the mouth.")
    )
     ]
    ['burnt-tree
      (p "The vengeful husk of the burnt tree, a vast deep black silhouette against the sky. Near ground, there's a slit in its outer shell, just wide enough for Otava to fit through. Inside, nothing but black and and an intense smell of ash.")]

    ['outpost
      (p "The building is a research outpost. Outside the main building, there's a big, drill-like machine puncturing the rock. Inside the building, there's more heavy machinery, a steel platform, a bench and some empty closets. Stairs lead down to a tunnel entrance.

      There is drying blood on the platform.

      There's also separate brick-walled room on ground level.")]
      )
  )

(define (describe-non-combat-situation repeated?)
  (cond ((null? (current-fragment-id))
         (cond ((eq? (location-id (current-location)) 'magpie-hill)
                (p #:suppress-logging? repeated? "Natural rock stairs lead back to Perimeter. There's a small, decrepit industrial-looking building further ahead on the plateau. A small trail leads along the edge of the plateau.")))
         (cond ((location-has-feature? (current-location) 'magpie-effigy)
                (p #:suppress-logging? repeated?"\"Chk-chk\", the magpie calls insistently from the foliage of the skeletonlike forest on the plateau.")))
         (describe-location repeated?)
                ))
  )


(define (describe-situation repeated?)
  (when (location-has-feature? (current-location) 'locked-door)
    (p "The door in the brick wall is locked with a heavy padlock."))
  (cond
    ((current-in-combat?) (describe-combat-situation))
    (else
      (describe-non-combat-situation repeated?)))
  (if (flag-set? 'perspective-switched)
    (p #:suppress-logging? repeated? "Otava is the space in which the world appears. Nothing is still, everything fluxating and pulsuating. Atomic particles thrown about by forces of cause and effect. A pulsating heartbeat emanates from deep within the earth's crust.")
    '())

  )

