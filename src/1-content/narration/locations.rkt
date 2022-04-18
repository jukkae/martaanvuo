#lang at-exp racket


(provide describe-begin-traverse-action
         describe-finish-traverse-action
         describe-cancel-traverse-action
         display-location-info-card
         get-traverse-text
         route-shortname)

(require "../../0-engine/0-api/api.rkt")

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
         "Otava begins the climb. The trail turns into a narrow natural staircase of rocks, as the hillside steepens to a cliff.")]
       [else
        (p
         "Otava climbs up Magpie Hill.")])
     ]
    ['(magpie-hill perimeter)
     (case n
       [(1)
        (p
         "The sharply-angled terraced rocks along the first, steepest part of the descent tear some new holes in Otava's pants.")]
       [else
        '()])
     ]
    ['(perimeter martaanvuo-swamp)
     (case n
       [(1)
        (p
         "The trail, following the ants, descends slightly, twisting and turning and eventually disappearing. The ground under her feet turns into soggy mud, and there are some reeds growing here and there. In the deepest troughs, there is some stinking, oily water.")]
       [else
        '()])
     ]
    ['(martaanvuo-swamp perimeter)
     (case n
       [(1)
        (p
         "The path comes to a muddy, oily swamp.")]
       [else
        '()])
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

    ['(martaanvuo-swamp burnt-tree)
     (p "Otava takes the small side path. Some of the stubby trees look partly burned. There's a dry, resinous, incense-like smoky smell in the air.")
     ]

    ['(burnt-tree martaanvuo-swamp)
     '()
     ]

    ['(burnt-tree the-maw)
     (p "Inside the big tree, it is cool and wet. The ground drops, and there's an earthen tunnel.")
     ]

    [else
     (dev-note (format "describe-begin-traverse-action: unknown key: ~a" key))
     ])
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
        (p "Otava comes to a slab of stone with a painting of a human figure on it. The being has the head of an ant, and its six arms are contorted in a mad dance, one pair of hands gesturing wildly with the control rods of a puppeteer. The bottom of the painting is washed away. The figure's unsettling gaze follows Otava as she goes past the painting.")]
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
         "A narrow, long overhang leads into the clear air.")]
       [else
        (p
         "Otava arrives at Luminous Precipice.")])
     ]

    ['(luminous-precipice magpie-hill)
     (case n
       [(1)
        (p
         "A trail leads along the edge of the plateau.")]
       [else
        (p
         "Otava arrives at Luminous Precipice.")])
     ]

    ['(perimeter martaanvuo-swamp)
     (case n
       [(1)
        (p
         "The path is visible again through the muck, and it splits in two.")
        ]
       [else
        '()])]
    ['(martaanvuo-swamp perimeter)
     '()]

    ['(martaanvuo-swamp martaanvuo-docks)
     (case n
       [(1)
        (p "Otava finds herself following a fresh-looking vehicle track. The smell of rotting fish fills the air, and a small village comes to view. A small, drying lake with oily water, a couple of sheds, meters out from the receding shoreline. A pile of fish by the dock explains the smell.")
        ]
       [else
        '()])]

    ['(martaanvuo-docks martaanvuo-swamp)
     (p
      "The tracks disappear amids a rocky outcrop, and eventually Otava finds herself at the fork in Martaanvuo swamp again.")
     ]

    ['(martaanvuo-swamp burnt-tree)
     (case n
       [(1)
        (p "A huge, angular hulk of burnt tree looms over Otava.")]
       [else
        (p
         "Otava comes to the burnt tree.")])]

    ['(burnt-tree martaanvuo-swamp)
     (p "The path leads Otava to the fork in Martaanvuo swamp.")
     ]

    [else
     (dev-note (format "describe-finish-traverse-action: unknown key: ~a" key))
     ]
    ))

(define (describe-cancel-traverse-action from to)

  (define key (list (location-id from) (location-id to)))
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
              "The rocky climb up.")]

         ['martaanvuo-swamp
          (if (route-fully-known? route)
              "Martaanvuo swamp."
              "The narrow path.")]
         [else (format "[go to: ~a]" (route-other-end-from route start-location))])]

      ['magpie-hill
       (case (route-other-end-from route start-location)
         ['perimeter
          (if (route-fully-known? route)
              "The rocky descent down to perimeter."
              "The steep descent.")]
         ['martaanvuo-swamp
          (if (route-fully-known? route)
              "The path to the fork in Martaanvuo swamp."
              "The downhill path.")]
         ['outpost
          (if (route-fully-known? route)
              "The outpost."
              "The building with the machine.")]
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
         ['burnt-tree
          (if (route-fully-known? route)
              "The path to the burnt tree."
              "The small side path to the right.")]
         ['martaanvuo-docks
          (if (route-fully-known? route)
              "The village."
              "The wider path straight ahead.")]
         [else (format "[go to: ~a]" (route-other-end-from route start-location))])]

      ['burnt-tree
       (case (route-other-end-from route start-location)
         ['martaanvuo-swamp
          (if (route-fully-known? route)
              "Back to Martaanvuo Swamp."
              "The lone path.")]
         ['the-maw
          (if (route-fully-known? route)
              "The slit to the Maw."
              "The slit.")]
         [else (format "[go to: ~a]" (route-other-end-from route start-location))])]

      ['martaanvuo-docks
       (case (route-other-end-from route start-location)
         ['martaanvuo-swamp
          (if (route-fully-known? route)
              "Back to Martaanvuo Swamp."
              "The vehicle trail out.")]
         [else (format "[go to: ~a]" (route-other-end-from route start-location))])]

      ['outpost
       (case (route-other-end-from route start-location)
         ['cache
          (if (route-fully-known? route)
              "The cache."
              "The previously locked door.")]
         ['sewers-1 "The sewers."] ; TODO: sewers -> tunnels
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
         (dev-note (format "location-info-card: unknown location: ~a" location)))))
