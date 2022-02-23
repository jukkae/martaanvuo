#lang at-exp racket


(provide describe-begin-traverse-action
         describe-finish-traverse-action
         describe-cancel-traverse-action
         display-location-info-card
         get-traverse-text
         route-shortname)

(require "0-types/location.rkt"
         "routes.rkt"
         "../core/api.rkt"
         "../actions/action.rkt")

(define (describe-begin-traverse-action route direction)
  (define from (if (eq? direction 'a-to-b)
                   (route-a route)
                   (route-b route)))
  (define to (if (eq? direction 'a-to-b)
                 (route-b route)
                 (route-a route)))
  (define key (list from to))
  (times-begin-traverse-narrated++ key) ; dumbass order of initialization but who cares
  (define n (times-begin-traverse-narrated key))
  (case key
    ['(perimeter magpie-hill)
     (case n
       [(1)
        (p
         "Drawn by the magpie's call, Otava begins her ascent. The trail turns into a narrow, natural staircase of rocks, as the hillside steepens to a cliff.")]
       [else
        (p
         "Otava climbs the natural stairs up to Magpie Hill.")])
     ]
    ['(magpie-hill perimeter)
     (case n
       [(1)
        (p
         "The sharply-angled terraced rocks along the first, steepest part of the descent tear some new holes in Otava's pants.")]
       [else
        (p
         "Otava prepares herself for the descent, careful this time.")])
     ]
    ['(perimeter martaanvuo-swamp)
     (case n
       [(1)
        (p
         "Otava follows the ants and takes the right-hand path. The trail descends slightly, twisting and turning and eventually disappearing. The ground under her feet turns soggy, and the smell of suopursu fills the air as the forested swamp turns into an open marsh.")]
       [else
        (p
         "As the trail disappears as she goes down towards the marsh, Otava notices some cracked bones under the underbrush.")])
     ]
    ['(martaanvuo-swamp perimeter)
     (case n
       [(1)
        (p
         "The path comes to a barely traversable marsh.")]
       [else
        (p
         "The marsh smells like suopursu.")])
     ]

    ['(martaanvuo-swamp martaanvuo-docks)
     (case n
       [(1)
        (p
         "The trail disappears for a bit in the middle of sharp rocks.")]
       [else
        (p
         "Otava comes to the sharp rocks.")])
     ]
    ['(martaanvuo-docks martaanvuo-swamp)
     (case n
       [(1)
        (p
         "The smell of rotting fish slowly disappears as Otava climbs up the vehicle trail.")]
       [else
        (p
         "Otava climbs up the vehicle trail, away from the docks and the river.")])
     ]

    ['(martaanvuo-swamp crematory)
     (p "Otava takes the small side path. Some of the trees look burned, and there's a lingering smell of acrid smoke and ash in the air.")
     ]

    ['(crematory martaanvuo-swamp)
     (p "Otava goes from the dreary lime courtyard of the crematory to the small path.")
     ]

    [else
     (dev-note "describe-begin-traverse-action: unknown key:")
     (displayln key)])
  )

(define (describe-finish-traverse-action route direction)
  (define from (if (eq? direction 'a-to-b)
                   (route-a route)
                   (route-b route)))
  (define to (if (eq? direction 'a-to-b)
                 (route-b route)
                 (route-a route)))
  (define key (list from to))
  (times-finish-traverse-narrated++ key) ; dumbass order of initialization but who cares
  (define n (times-finish-traverse-narrated key))
  (case key
    ['(perimeter magpie-hill)
     (case n
       [(1)
        (p "Otava comes to a slab of stone with a painting of a human figure on it. The being has the head of an ant, and its six arms are contorted in a mad dance, one pair of hands gesturing wildly with the control rods of a puppeteer. The bottom of the painting is washed away. The figure's unsettling gaze follows Otava as she goes past the painting and fills her with apprehension. It begins to drizzle.")
        (p "Soon after the dreadful painting, the rocky stairs turn back to a trail that levels out. The thick fog and the drizzle that's now a steady rain obscure much of the view. Otava is at the edge of a large plateau. The silhouette of a decaying industrial building looms in the distance. Is this it? The Facility?")]
       [else
        (p
         "The dreadful painting of Anthead God gazes at Otava mockingly as she passes the painted stone slab.")])]

    ['(magpie-hill perimeter)
     (case n
       [(1)
        (p
         "Eventually, the boulders get smaller, and the path evens out.")]
       [else
        (p
         "Otava gets to Perimeter.")])
     ]

    ['(magpie-hill luminous-precipice)
     (case n
       [(1)
        (p
         "A narrow, long overhang leads into the mist that has acquired a soft milky glow.")]
       [else
        (p
         "Otava arrives at Luminous Precipice.")])
     ]

    ['(luminous-precipice magpie-hill)
     (case n
       [(1)
        (p
         "A trail leads along the edge of the plateau into deepening fog.")]
       [else
        (p
         "Otava arrives at Luminous Precipice.")])
     ]

    ['(perimeter martaanvuo-swamp)
     (case n
       [(1)
        (p
         "After the barely-traversable marsh, Otava finds a footpath to follow once again. Soon, the hills come closer to her on her left again, and there is a fork in the path.")
        ]
       [else
        (p
         "Otava comes to Martaanvuo Fork.")])]
    ['(martaanvuo-swamp perimeter)
     (case n
       [(1)
        (p
         "The path begins to rise, and ground turns more solid. Otava gets to Perimeter.")
        ]
       [else
        (p
         "Otava comes to Perimeter.")])]

    ['(martaanvuo-swamp martaanvuo-docks)
     (case n
       [(1)
        (p "Otava finds herself following a fresh-looking vehicle track. The smell of rotting fish fills the air, and old, decaying docks slowly appear from the fog. There's a secluded cove, surrounded by cliffs, a couple of sheds by the shoreline, and a few berths. A small fishing boat with a pile of fish on the deck explains the smell.")
        (p "Through the fog, Otava sees a stilted figure moving spastically on the water maybe fifteen meters away from the shore. She watches the figure from behind a webbing that's hanging from a winch. The figure is fishing, throwing a bait and then dragging the catch in with sudden, jerky motions.")]
       [else
        (p
         "The docks slowly appear from the mist.")])]

    ['(martaanvuo-docks martaanvuo-swamp)
     (p
      "The tracks disappear amids a rocky outcrop, and eventually Otava finds herself at the fork in Martaanvuo swamp again.")
     ]

    ['(martaanvuo-swamp crematory)
     (case n
       [(1)
        (p "The ruins of a lime-white stone building appear from the mist. There's a row of metal rings high along one wall, and the wall is stained dark under the rings.")]
       [else
        (p
         "Otava comes to the Crematory.")])]

    ['(crematory martaanvuo-swamp)
     (p "The path leads Otava to the fork in Martaanvuo swamp.")
     ]

    [else
     (dev-note "describe-finish-traverse-action: unknown key:")
     (displayln key)]
    ))

(define (describe-cancel-traverse-action from to)

  (define key (list from to))
  (times-cancel-traverse-narrated++ key) ; dumbass order of initialization
  (define n (times-cancel-traverse-narrated key))
  (case key
    ['(perimeter magpie-hill)
     (case n
       [(1)
        (p "Otava comes back to the fork in the path somewhere in Perimeter.")]
       [else
        (p "Otava comes back to the fork in Perimeter.")])
     ])
  )

(define (get-traverse-text route start-location)
  (define direction
    (cond ((eq? (location-id start-location)
                (route-a route))
           'a-to-b)
          ((eq? (location-id start-location)
                (route-b route))
           'b-to-a)))

  (define (get-route-short-description)

    (case (location-id start-location)

      ['perimeter
       (case (route-other-end-from route start-location)

         ['magpie-hill
          (if (route-fully-known? route)
              "Magpie Hill."
              "Rocky slope.")]

         ['martaanvuo-swamp
          (if (route-fully-known? route)
              "Martaanvuo Swamp."
              "Soggy downhill path.")]
         [else (format "[go to: ~a]" (route-other-end-from route start-location))])]

      ['magpie-hill
       (case (route-other-end-from route start-location)
         ['perimeter
          (if (route-fully-known? route)
              "Rocky stairs to Perimeter"
              "The steep descent.")]
         ['martaanvuo-swamp
          (if (route-fully-known? route)
              "The path to the fork in Martaanvuo Swamp."
              "The downhill path.")]
         ['power-plant-ruins
          (if (route-fully-known? route)
              "The power plant ruins."
              "The decrepit building.")]
         ['luminous-precipice
          (if (route-fully-known? route)
              "Luminous Precipice."
              "The trail along the edge of the plateau.")]
         [else (format "[go to: ~a]" (route-other-end-from route start-location))])]

      ['martaanvuo-swamp
       (case (route-other-end-from route start-location)
         ['magpie-hill
          (if (route-fully-known? route)
              "Magpie Hill."
              "The path uphill and to the left.")]
         ['perimeter
          (if (route-fully-known? route)
              "Go back to Perimeter."
              "The lone path away from the fork.")]
         ['crematory
          (if (route-fully-known? route)
              "Crematory."
              "The small side path to the right.")]
         ['martaanvuo-docks
          (if (route-fully-known? route)
              "The Docks."
              "The broader path straight ahead.")]
         [else (format "[go to: ~a]" (route-other-end-from route start-location))])]

      ['crematory
       (case (route-other-end-from route start-location)
         ['martaanvuo-swamp
          (if (route-fully-known? route)
              "Back to Martaanvuo Swamp."
              "The lone path.")]
         [else (format "[go to: ~a" (route-other-end-from route start-location))])]

      ['martaanvuo-docks
       (case (route-other-end-from route start-location)
         ['martaanvuo-swamp
          (if (route-fully-known? route)
              "Back to Martaanvuo Swamp."
              "The vehicle trail out.")]
         [else (format "[go to: ~a]" (route-other-end-from route start-location))])]

      ['power-plant-ruins
       (case (route-other-end-from route start-location)
         ['cache
          (if (route-fully-known? route)
              "The cache."
              "The previously locked door.")]
         ['sewers-1 "The sewers."]
         ['magpie-hill "Go outside."]
         [else (format "[go to: ~a]" (route-other-end-from route start-location))])]

      [else (format "[go to: ~a]" (route-other-end-from route start-location))]))

  #;(cond ((route-fully-known? route)
           ; Currently, "fully known" implies having been at the other end, fix as needed
           (case direction
             ['a-to-b
              (define to-name (get-location-name-from-location (route-b route)))
              (format "Go back to ~a." to-name) ; breaks eg. when recursing into a new run
              ]
             ['b-to-a
              (define to-name (get-location-name-from-location (route-a route)))
              (format "Go back to ~a." to-name) ; breaks eg. when recursing into a new run
              ]))
          (else
           (get-route-short-description)))
  (get-route-short-description))


(define (display-route-info-card route)
  (define id (location-id route))
  (define title "Location (en route)")

  (define pending-action (current-pending-action))
  (define details (action-details pending-action))

  (define traverse-direction
    (if (memq 'a-to-b details)
        'a-to-b
        'b-to-a))

  (define endpoint
    (case traverse-direction
      ['a-to-b (route-b route)]
      ['b-to-a (route-a route)]))

  (define startpoint
    (case traverse-direction
      ['a-to-b (route-a route)]
      ['b-to-a (route-b route)]))

  (define body
    (cond ((route-fully-known? route)
           (prune
            (tbody
             (tr
              (format "~a – ~a" (place-shortname startpoint) (place-shortname endpoint))
              "[route]")
             (when (not (null? (location-features route)))
               (tr "features"
                   (~v (location-features route)))))))
          (else
           (prune
            (tbody
             (tr
              (format "~a – ???" (place-shortname startpoint))
              "[route]")
             (when (not (null? (location-features route)))
               (tr "features"
                   (~v (location-features route)))))))))
  (info-card body title))

(define (display-place-info-card location [title "Location"])
  (define id (location-id location))
  (define body
    (prune (tbody
            (when (not (eq? (place-shortname location) ""))
              (tr (place-shortname location)
                  " "))
            (when (not (null? (location-id location)))
              (tr "id"
                  (cond ((number? id) (number->string id))
                        ((symbol? id) (symbol->string id)))))
            (when (and (null? (location-id location))
                       (not (null? (location-type location))))
              (tr "type"
                  (symbol->string (location-type location))))
            (when (not (null? (location-items location)))
              (tr "items"
                  (~v (location-items location))))
            (when (not (null? (location-features location)))
              (tr "features"
                  (~v (location-features location))))
            )))
  (info-card body title))

(define (display-location-info-card location [title "Location"])
  (cond ((place? location)
         (display-place-info-card location))
        ((route? location)
         (display-route-info-card location))
        (else
         (displayln "location-info-card: unknown location:")
         (displayln location))))
