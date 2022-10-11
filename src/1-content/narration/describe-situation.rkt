#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../../0-engine/0-api/api.rkt"
  )

(lazy-require ["combat-narration.rkt"
 (describe-combat-situation
  )])

(define (describe-location repeated?)
  '()#;(case (location-id (current-location))
    ['burnt-tree
     (p "The vengeful husk of the burnt tree, a vast deep black silhouette against the sky. Near ground, there's a slit in its outer shell, just wide enough for Otava to fit through. Inside, nothing but black and and an intense smell of ash.")]

    ['outpost
     (p "The building is a research outpost. Outside the main building, there's a big, drill-like machine puncturing the rock. Inside the building, there's more heavy machinery, a steel platform, a bench and some empty closets. Stairs lead down to a tunnel entrance.

      There is drying blood on the platform.

      There's also separate brick-walled room on ground level.")]
    )
  ; TODO: this -> content
  (define body
    (case (location-id (current-location))
      ['perimeter
        (define unpruned-rows '())
       (when (pc-has-sense-organ? 'nose)
         (set! unpruned-rows
               (append-element unpruned-rows
                               (tr
                                "Smells      [perceived with nose]"
                                "Noxious smell of rotting flesh")))
         )
       (when (pc-has-sense-organ? 'ears)
         (set! unpruned-rows
               (append-element unpruned-rows
                               (tr
                                "Sounds      [perceived with ears]"
                                "A magpie cries somewhere above.")))
         )
        unpruned-rows
        ]
      ['magpie-hill
       (define unpruned-rows '())

       (when (pc-has-sense-organ? 'nose)
         (set! unpruned-rows
               (append-element unpruned-rows
                               (tr
                                "Smells      [perceived with nose]"
                                "Arid smell of desert flowers")))
         )
       (when (pc-has-sense-organ? 'ears)
         (set! unpruned-rows
               (append-element unpruned-rows
                               (tr
                                "Sounds      [perceived with ears]"
                                ;"Chk-chk, a magpie calls insistently"
                                "The magpie is now silent. The wind whistles."
                                )))
         )
        unpruned-rows
        ]
      ; TODO: Re-enable this
      ; [else (dev-note "TODO: Fix place descriptions!") '()]
      [else '()]
      )
    )

  (info-card
   body
   (cond
    [(Place? (current-location)) (format "Otava is in ~a." (Place-shortname (current-location)))]
    [else (format "Otava is in ~a." (route-shortname (current-location)))])
   )
  )

(define (describe-non-combat-situation repeated?)
  (cond ((null? (current-fragment-id))
         #;(cond ((equal? (location-id (current-location)) 'magpie-hill)
                (p #:suppress-logging? repeated? "Natural rock stairs lead back to Perimeter. There's a small, decrepit industrial-looking building further ahead on the plateau. A small trail leads along the edge of the plateau.")))
         (describe-location repeated?)
                ))
  )


(define (describe-situation repeated?)
  ; (when (location-has-feature? (current-location) 'locked-door)
  ;   (p "The door in the brick wall is locked with a heavy padlock."))
  (cond
    ((current-in-combat?) (describe-combat-situation repeated?))
    (else
      (describe-non-combat-situation repeated?)))
  ; (if (flag-set? 'perspective-switched)
  ;   (p #:suppress-logging? repeated? "Otava is the space in which the world appears. Nothing is still, everything fluxating and pulsuating. Atomic particles thrown about by forces of cause and effect. A pulsating heartbeat emanates from deep within the earth's crust.")
  ;   '())
  )

