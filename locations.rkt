#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "action.rkt")
(require "io.rkt")
(require "location.rkt")
(require "route.rkt")
(require "utils.rkt")

(lazy-require ["situation.rkt"
               (current-location
                times-begin-traverse-narrated
                times-begin-traverse-narrated++
                times-finish-traverse-narrated
                times-finish-traverse-narrated++
                times-cancel-traverse-narrated
                times-cancel-traverse-narrated++
                set-flag
                next-chapter!)])

(lazy-require ["decision.rkt"
               (decision
                make-decision)])

(lazy-require ["pc.rkt"
               (add-item!)])

(lazy-require ["quests.rkt"
               (create-quest)])


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
        (paragraph
         "Drawn by the magpie's call, Otava begins her ascent. The trail turns into a narrow, natural staircase of rocks, as the hillside steepens to a cliff.")]
       [else
        (paragraph
         "Otava climbs the natural stairs up to Magpie Hill.")])
     ]
    ['(perimeter martaanvuo-swamp)
     (case n
       [(1)
        (paragraph
         "Otava decides to follow the ants.")]
       [else
        (paragraph
         "Otava follows the ants.")])
     ])
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
        (paragraph "Otava comes to a slab of stone with a painting of a human figure on it. The being has the head of an ant, and its six arms are contorted in a mad dance, one pair of hands gesturing wildly with the control rods of a puppeteer. The bottom of the painting is washed away. The figure's unsettling gaze follows Otava as she goes past the painting and fills her with apprehension. It begins to drizzle.")
        (paragraph "Soon after the dreadful painting, the rocky stairs turn back to a trail that levels out. The thick fog and the drizzle that's now a steady rain obscure much of the view. Otava is at the edge of a large plateau. The silhouette of a decaying industrial building looms in the distance. Is this it? The Facility?")]
       [else
        (paragraph
         "The dreadful painting of Anthead God gazes at Otava mockingly as she passes the painted stone slab.")])]
    
    ['(martaanvuo-swamp martaanvuo-docks)
     (case n
       [(1)
        (paragraph "Decaying docks slowly appear from the mist, and the smell of rotting fish gets stronger, until it is almost unbearable. Rusty rebar sticks out from vast broken concrete slabs. Most portside buildings are completely ruined, but some piers are still in one piece. Rebar steel looks good for welding – if only she could transport it.")
        (paragraph "Through the fog, Otava sees a stilted figure moving spastically on the water maybe fifteen meters away from the shore. She watches the figure from behind a pile of rubble and rusty rebar. It is fishing, throwing a bait and then dragging the catch in with sudden, jerky motions.")]
       [else
        (paragraph
         "The docks slowly appear from the mist.")])]

    [else
     (displayln "unknown key:")
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
        (paragraph "Otava comes back to the fork in the path somewhere in Perimeter.")]
       [else
        (paragraph "Otava comes back to the fork in Perimeter.")])

     
     ])
  )


; This seems to be more content than code, so it's here for now, instead of location.rkt
(define (location-on-enter! location)
  (displayln "LOCATION-ON-ENTER")
  (displayln location))


(define (get-location-decisions location)
  (condense (list
             (when (location-has-feature? location 'stiltman)
               (make-decision
                #:title "Talk to the stilted figure."
                #:on-resolve! (proc
                        (next-chapter!)
                        (paragraph "Otava goes closer. The figure is a man on stilts, flailing peculiarly above the water, barely staying upright.  \"– helped me – no, I insist – finally you understand, working it through took ages – I am unstably present –\", shadowlike Stiltman stutters when it notices Otava, stumbling and wobbling in the mire. The man is wearing ragged overalls of a lab technician, and some sort of a makeshift semi-transparent mask covers his face, too foggy to see through. The logo on the overalls says Murkwater–Aegis.")
                        (remove-feature-from-location! location 'stiltman)
                        (paragraph "\"Uh, –\" Otava begins, but Stiltman goes on. \"– Anthead Monograph had the missing viewpoint on multi-valued logic which was needed to run the simulations  – I knew you would ask that question – here's the fee we agreed – or was, really –\"")
                        (paragraph "Stiltman throws something on the pier, and it lands with the metal clink of coins. Otava kneels to open the bag, while watching Stiltman. There's a handful of small gold coins in the bag.")
                        (add-item! 'gold #:amount 11 #:title "Picked up")
                        (create-quest 'anthead-monograph)
                        (set-flag 'ending-run-allowed))
                #:next-fragment 'stiltman-dialogue
                ))))

  ; not sure which is more ergonomic
  #;(case (location-id location)
      ['martaanvuo-docks
       (list
        (make-decision
         #:title "Talk to the stilted figure."
         #:description "\"... helped me – finally you understand – Murkwater made it special – here's the fee we agreed –\", the figure stutters, as it stumbles and wobbles in the mire. It drops something next to Otava. \"– yes, no, no – it is precisely 101 grams – \""
         #:on-resolve! (proc
                        (displayln "There is a bag of gold on the ground."))
         #:next-fragment 'exit
         ))]
      [else '()]))