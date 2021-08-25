#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "api.rkt")

(require "action.rkt"
         "blindscraper.rkt"
         "grabberkin.rkt"
         "location.rkt"
         "place.rkt")


(lazy-require ["state/state.rkt"
               (current-location
                times-begin-traverse-narrated
                times-begin-traverse-narrated++
                times-finish-traverse-narrated
                times-finish-traverse-narrated++
                times-cancel-traverse-narrated
                times-cancel-traverse-narrated++
                set-flag
                next-chapter!
                quest-exists?)])


(define (describe-begin-traverse-action action)
  (define from
    (cond ((route? (action-target action))
           (if (memq 'a-to-b (action-details action))
               (route-a (action-target action))
               (route-b (action-target action))))
          (else
           (current-location))
          ))

  (define to
    (cond ((route? (action-target action))
           (if (memq 'a-to-b (action-details action))
               (route-b (action-target action))
               (route-a (action-target action))))
          (else
           (action-target action))
          ))

  (define key (list (location-id from) (location-id to)))
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
    ['(perimeter martaanvuo-swamp)
     (case n
       [(1)
        (p
         "Otava follows the ants and takes the right-hand path. The trail descends slightly, twisting and turning and eventually disappearing. The ground under her feet turns soggy, and the smell of wild rosemary fills the air as the forested swamp turns into an open marsh.")]
       [else
        (p
         "As the trail disappears as she goes down towards the marsh, Otava notices some cracked bones under the underbrush.")])
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
     '()
     ]
    
    [else
     (dev-note "describe-begin-traverse-action: unknown key:")
     (displayln key)])
  )

(define (describe-finish-traverse-action action)
  (define from
    (cond ((route? (action-target action))
           (if (memq 'a-to-b (action-details action))
               (route-a (action-target action))
               (route-b (action-target action))))
          (else
           (current-location))
          ))

  (define to
    (cond ((route? (action-target action))
           (if (memq 'a-to-b (action-details action))
               (route-b (action-target action))
               (route-a (action-target action))))
          (else
           (action-target action))
          ))

  (define key (list (location-id from) (location-id to)))
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

    ['(perimeter martaanvuo-swamp)
     (case n
       [(1)
        (p
         "After the barely-traversable marsh, Otava finds a footpath to follow once again. Soon, the hills come closer to her on her left again, and there is a fork in the path.")
        ]
       [else
        (p
         "Otava comes to Martaanvuo Fork.")])]
    
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

    [else
     (dev-note "describe-finish-traverse-action: unknown key:")
     (displayln key)]

    

    ))
  


(define (describe-cancel-traverse-action action)
  (define from
    (cond ((route? (action-target action))
           (if (memq 'a-to-b (action-details action))
               (route-a (action-target action))
               (route-b (action-target action))))
          (else
           (current-location))
          ))

  (define to
    (cond ((route? (action-target action))
           (if (memq 'a-to-b (action-details action))
               (route-b (action-target action))
               (route-a (action-target action))))
          (else
           (action-target action))
          ))

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


; This seems to be more content than code, so it's here for now, instead of location.rkt
(define (location-on-enter! location)
  (displayln "LOCATION-ON-ENTER")
  (displayln location))


(define (get-location-decisions location)
  (condense (list

             ; definition / content goes to -> features, or world, or something
             (when (location-has-feature? location 'stiltman)
               (define manuscript-quest (quest-exists? 'anthead-monograph))
               (cond ((not manuscript-quest)
                      (make-decision
                       #:title "Talk to the stilted figure."
                       #:on-resolve! (proc
                                      (p "Otava goes closer to the figure flailing peculiarly above water. It turns out to be a man, balancing precariously on an insectlike, three-legged contraption of rods and springs and wire."))
                       #:next-fragment 'begin-stiltman-dialogue
                       ))
                     (else
                      (make-decision
                       #:title "Talk to Stiltman."
                       #:on-resolve! (proc
                                      (p "Stiltman flickers and flails above water, and Otava shouts out to him."))
                       #:next-fragment 'stiltman-continue-dialogue
                       ))))

             (when (location-has-feature? location 'martaanvuo-console)
               (make-decision
                #:title "Turn on the terminal."
                #:on-resolve! (proc
                               (p "Otava turns on the terminal. It clicks and whirrs, then the display comes to life."))
                #:next-fragment 'turn-on-martaanvuo-terminal
                ))

             )))



(define (spawn-enemies location)
  (define encounter-types '(blindscraper grabberkin))

  (define
    encounter-type
    (cond ((place? location)
           (cond ((eq? (place-type location) 'ridges)
                  'blindscraper)
                 ((eq? (place-type location) 'valleys)
                  'grabberkin)
                 (else (take-random encounter-types))))
          ((route? location)
           'blindscraper)))

  (case encounter-type
    ['grabberkin
     (spawn-grabberkin-encounter!)
     ]
    ['blindscraper
     (spawn-blindscraper-encounter!)
     ]
    ))