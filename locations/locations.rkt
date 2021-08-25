#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "location.rkt"
         "place.rkt"
         "route.rkt")

(require "../action.rkt")


#;(lazy-require
 ["state/state.rkt"
  (get-pending-traverse-direction)])

(require "../api.rkt"
         "../actor.rkt")

(require "../blindscraper.rkt"
         "../grabberkin.rkt")


#;(lazy-require ["../state/state.rkt"
               (current-location
                times-begin-traverse-narrated
                times-begin-traverse-narrated++
                times-finish-traverse-narrated
                times-finish-traverse-narrated++
                times-cancel-traverse-narrated
                times-cancel-traverse-narrated++
                set-flag
                quest-exists?)])

#;(lazy-require ["../state/logging.rkt"
               (next-chapter!)])


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
     (p "Otava takes the small side path. Some of the trees look burned, and there's a lingering smell of acrid smoke and ash in the air.")
     ]

    ['(crematory martaanvuo-swamp)
     (p "Otava goes from the dreary lime courtyard of the crematory to the small path.")
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

    ['(crematory martaanvuo-swamp)
     (p "The path leads Otava to the fork in Martaanvuo swamp.")
     ]

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
           (cond ((eq? (location-type location) 'ridges)
                  'blindscraper)
                 ((eq? (location-type location) 'valleys)
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



; internal
(define (get-location-short-description location)
  (define name
    (cond ((place? location)
           (place-shortname location))
          ((route? location)
           (route-shortname location))
          ))
  (define features-str
    ; Disabled for now, just do empty string
    #;(cond ((not (null? (location-features location)))
             (cond ((memq 'magpie-effigy (location-features location))
                    "Magpie Effigy")
                   (else "Unknown features TODO")))
            (else ; no features
             ""))
    "")
  (string-append name
                 features-str)
  )


(define (display-location-info-card location [title "Location"])
  (cond ((place? location)
         (display-place-info-card location))
        ((route? location)
         (display-route-info-card location))
        (else
         (displayln "location-info-card: unknown location:")
         (displayln location))))


(define (move-pc-to-location! location)
  ; TODO: location on-exit / on-enter triggers here
  #;(displayln (string-append "-- move-pc-to-location!: moving to " (~v location)))
  (remove-actor-from-its-current-location! (pc))
  (set-actor-location! (pc) location)
  (add-actor-to-location! location (pc))
  (when (place? location)
    (set-place-visited?! location #t)
    (for ([route (place-routes location)])
      (when #t ; if not hidden
        (set-route-endpoint-visited! route location)
        ))
      
    ))


(define (location-neighbors location)
  (cond ((route? location)
         (list
          (route-a location)
          (route-b location)))
        ((place? location)
         (place-routes location))))


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
            (list
             (list
              (string-append " "
                             (place-shortname startpoint)
                             " – "
                             (place-shortname endpoint)
                             " ")
              (string-append " "
                             "[route]"
                             " "))
             (when (not (null? (location-features route)))
               (list (string-append " "
                                    "features"
                                    " ")
                     (string-append " "
                                    (~v (location-features route))
                                    " "))))))
          (else
           (prune
            (list
             (list
              (string-append " "
                             (place-shortname startpoint)
                             " – "
                             "???"
                             " ")
              (string-append " "
                             "[route]"
                             " "))
             (when (not (null? (location-features route)))
               (list (string-append " "
                                    "features"
                                    " ")
                     (string-append " "
                                    (~v (location-features route))
                                    " "))))))))
  (info-card body title))

(define (display-place-info-card location [title "Location"])
  (define id (location location))
  (define body
    (prune (list
            (when (not (eq? (place-shortname location) ""))
              (list (string-append " "
                                   (place-shortname location)
                                   " ")
                    "  "))
            (when (not (null? (location-id location)))
              (list (string-append " "
                                   "id"
                                   " ")
                    (string-append " "
                                   (cond ((number? id) (number->string id))
                                         ((symbol? id) (symbol->string id)))
                                   " ")))
            (when (and (null? (location-id location))
                       (not (null? (location-type location))))
              (list (string-append " "
                                   "type"
                                   " ")
                    (string-append " "
                                   (symbol->string (location-type location))
                                   " ")))
            (when (not (null? (location-items location)))
              (list (string-append " "
                                   "items"
                                   " ")
                    (string-append " "
                                   (~v (location-items location))
                                   " ")))
            (when (not (null? (location-features location)))
              (list (string-append " "
                                   "features"
                                   " ")
                    (string-append " "
                                   (~v (location-features location))
                                   " ")))
            )))
  (info-card body title))



(define (get-traverse-text route start-location)
  (define direction
    (cond ((eq? (location-id start-location)
                (location-id (route-a route)))
           'a-to-b)
          ((eq? (location-id start-location)
                (location-id (route-b route)))
           'b-to-a)))

  (define (get-route-short-description)

    (case (location-id start-location)

      ['perimeter
       (case (location-id (route-other-end-from route start-location))

         ['magpie-hill
          (if (route-fully-known? route)
              "Rocky stairs up Magpie Hill."
              "Magpie and the rocky slope.")]
         
         ['martaanvuo-swamp
          (if (route-fully-known? route)
              "Martaanvuo Swamp."
              "Ants and the swamp.")]
         [else (string-append "["
                              "go to: "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]
      
      ['magpie-hill
       (case (location-id (route-other-end-from route start-location))
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
         [else (string-append "["
                              "go to: "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]
      
      ['martaanvuo-swamp
       (case (location-id (route-other-end-from route start-location))
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
         [else (string-append "["
                              "go to: "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]

      ['crematory
       (case (location-id (route-other-end-from route start-location))
         ['martaanvuo-swamp
          (if (route-fully-known? route)
              "Back to Martaanvuo Swamp."
              "The lone path.")]
         [else (string-append "["
                              "go to: "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]

      ['martaanvuo-docks
       (case (location-id (route-other-end-from route start-location))
         ['martaanvuo-swamp
          (if (route-fully-known? route)
              "Back to Martaanvuo Swamp."
              "The vehicle trail out.")]
         [else (string-append "["
                              "go to: "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]
      
      ['power-plant-ruins
       (case (location-id (route-other-end-from route start-location))
         ['cache "The previously locked door."]
         ['sewers-1 "The sewers."]
         [else (string-append "["
                              "go to: "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]

      [else (string-append "["
                           "go to: "
                           (symbol->string (location-id (route-other-end-from route start-location)))
                           "]")]))
  

  
  #;(cond ((route-fully-known? route)
           ; Currently, "fully known" implies having been at the other end, fix as needed
           (case direction
             ['a-to-b
              (define to-name (get-location-name-from-location (route-b route)))
              (string-append "Go back to " to-name ".") ; breaks eg. when recursing into a new run
              ]
             ['b-to-a
              (define to-name (get-location-name-from-location (route-a route)))
              (string-append "Go back to " to-name ".") ; breaks eg. when recursing into a new run
              ]))
          (else
           (get-route-short-description)))
  (get-route-short-description))

(define (route-other-end-from route start-location)
  (define start
    (cond ((eq? (location-id start-location)
                (location-id (route-a route)))
           'a)
          ((eq? (location-id start-location)
                (location-id (route-b route)))
           'b)))
  (define endpoint
    (case start
      ['a (route-b route)]
      ['b (route-a route)]))
  endpoint)



(define (set-route-endpoint-visited! route location)
  (define endpoint
    (cond ((eq? (location-id location)
                (location-id (route-a route)))
           'a)
          ((eq? (location-id location)
                (location-id (route-b route)))
           'b)))
  (case endpoint
    ['a (add-detail-to-location! route 'a-visited)]
    ['b (add-detail-to-location! route 'b-visited)]))


(define (route-place-known? route place)
  (define endpoint
    (cond ((eq? (location-id place)
                (location-id (route-a route)))
           'a)
          ((eq? (location-id place)
                (location-id (route-b route)))
           'b)))
  (case endpoint
    ['a (place-visited? (route-a route))]
    ['b (place-visited? (route-b route))])
  )

(define (route-shortname route)
  (define direction (get-pending-traverse-direction))

  (define startpoint
    (case direction
      ['a-to-b (route-a route)]
      ['b-to-a (route-b route)]))
  (define endpoint
    (case direction
      ['a-to-b (route-b route)]
      ['b-to-a (route-a route)]))

  
  
  (cond ((route-fully-known? route)
         (string-append "En route: "
                        (place-shortname startpoint)
                        " – "
                        (place-shortname endpoint)
                        " "))
        (string-append "En route: "
                       (place-shortname startpoint)
                       " – "
                       "???"
                       " ")))