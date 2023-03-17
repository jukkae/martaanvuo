#lang at-exp racket

(provide (all-defined-out))
(provide (all-from-out "routes.rkt"))

(require racket/lazy-require
         "../pc/pc.rkt"
         "../world/time.rkt"
         "../../3-types/action.rkt"
         "../../3-types/choice.rkt"
         "../../3-types/clue.rkt"
         "../../3-types/sense-organ.rkt"
         "../../3-types/item.rkt"
         "../../3-types/zone.rkt")

(lazy-require ["../world/world.rkt"
               (remove-actor-from-its-current-location! get-place-by-id get-route-by-id get-route-between get-current-light-level pc)])
(lazy-require ["../simulation.rkt" (advance-time-until-next-interesting-event!)])
(lazy-require ["../checks/checks.rkt" (check)])
(lazy-require ["../items/item.rkt" (make-item)])
(lazy-require ["../../6-combat/combat.rkt" (begin-combat!)])
(lazy-require ["../../7-state/mutators.rkt" (current-location)])
(lazy-require ["../../7-state/pending-action.rkt" (reset-pending-action!)])

(lazy-require ["../../../1-content/world/locations/location-choices.rkt" (get-location-choices)])

(lazy-require ["../../../1-content/narration/routes.rkt" (get-traverse-text)])

(lazy-require ["../../../1-content/encounters/encounters.rkt"
               (spawn-markbearer-encounter! spawn-grabberkin-encounter!
                                            spawn-blindscraper-encounter!
                                            spawn-two-blindscrapers-encounter!
                                            spawn-grabberkin-and-blindscraper-encounter!
                                            spawn-voidfloater-encounter!
                                            spawn-limbtearer-encounter!)])

(require "routes.rkt"
         "../actors/actor.rkt"

         "../../1-index/state.rkt"

         "../../2-core/io.rkt"
         "../../2-core/core.rkt"

         "../../3-types/decision.rkt"
         "../../3-types/location-ids.rkt"
         "../../3-types/location.rkt"
         "../../3-types/place.rkt"
         "../../3-types/route.rkt"
         "../../3-types/actor.rkt")

(define (location-on-enter! location)
  #;(dev-note (format "location-on-enter! tbd for location ~a" (location-id location)))
  (case (location-id location)
    ['magpie-hill
     (when (not (flag-set? 'magpie-hill-climbed))
       (set-flag 'magpie-hill-climbed)
       (p
        "Up on Magpie Hill there is a plateau, and on that plateau, there is a [ritual circle]. In the ritual circle, one can perform the Ritual of Translocation and enter the Maw.")
       (wait-for-confirm))]
    ['lookout
     (cond
       [(pc-has-sense-organ? 'eyes)
        (cond
          [(not (equal? (get-current-light-level) 'pitch-black))
           (when (not (flag-set? 'lookout-visited))
             (set-flag 'lookout-visited)
             (p "The view from the lookout is impressive. The massive [dam] looks like a miniature, keeping back the waters from the [dried basin]. There's also two small [villages] along the mostly dried Martaanvuo riverbed, maybe a half-day walk from the dam. Far in the distance a stretch of highway stretches in the distance.")

             (define (reveal-route route)
              (cond [(not (null? route))
                     (set-route-hidden?! route #f)
                     (notice (format "Route revealed: [~a] – [~a] – [~a]" (Place-shortname (get-place-by-id (route-a route))) (route-descr-from-a route) (Place-shortname (get-place-by-id (route-b route)))))
                     ]))

             (reveal-route (get-route-between 'perimeter 'martaanvuo-dam))
             (reveal-route (get-route-between 'martaanvuo-dam 'martaanvuo-basin))(reveal-route (get-route-between 'martaanvuo-basin 'martaanvuo-river))
             (reveal-route (get-route-between 'martaanvuo-river 'abandoned-village))
             (reveal-route (get-route-between 'abandoned-village 'village))
             (award-xp! 2 "for gaining some perspective")

             (wait-for-confirm))]
          [else
           (p "It's too fucking dark to see anything.")])]
       [else
        (p "Man, she should come back when she's got eyes, the view must be great from here.")])]))

(define (get-location-decisions location)
  (condense
   ; definition / content goes to -> features, or world, or something
   (list (when (location-has-feature? location 'martaanvuo-terminal)
           (make-decision
            #:title "Turn on the terminal."
            #:next-fragment
            (thunk
             (p "Otava turns on the terminal. It clicks and whirrs, then the display comes to life.")
             'turn-on-martaanvuo-terminal))))))

(define (spawn-enemies encounter-type)
  ; TODO: move to encounter-specific content
  (case encounter-type
    ['markbearer (spawn-markbearer-encounter!)]
    ['grabberkin (spawn-grabberkin-encounter!)]
    ['blindscraper (spawn-blindscraper-encounter!)]
    ['two-blindscrapers (spawn-two-blindscrapers-encounter!)]
    ['grabberkin-and-blindscraper (spawn-grabberkin-and-blindscraper-encounter!)]
    ['voidfloater (spawn-voidfloater-encounter!)]
    ['limbtearer (spawn-limbtearer-encounter!)]
    [else (dev-note (format "Unknown encounter type: ~a" encounter-type))]))

(define (spawn-encounter)
  (current-counters++ 'enemy-encounters)

  (when (not (null? (location-encounter-types (current-location))))
    (spawn-enemies (take-random (location-encounter-types (current-location))))))

(define (get-location-name location)
  (format "~a"
          (cond
            [(Place? location)
             (if (equal? (Place-shortname location) "")
                 (capitalize-first-letter (string-replace (~a (location-id location)) "-" " "))
                 (Place-shortname location))]
            [(route? location) (route-shortname (location-id location))])))

(define (get-location-short-description location)
  (format "~a~a"
          (get-location-name location)
          (cond
            [(current-zone) (format ", ~a" (Zone-description (current-zone)))]
            [else ""])
          #;(cond
            [(and (or (pc-has-sense-organ? 'eyes) (pc-has-sense-organ? 'echolocation))
                  (not (null? (location-size (current-location)))))
             (format " [~a space]" (location-size (current-location)))]
            [else ""])))

(define (move-pc-to-location! location)
  (reset-pending-action!)
  (define old-location-id (if (not (null? (current-location))) (location-id (current-location)) '()))
  (remove-actor-from-its-current-location! (pc))
  (set-actor-location-id! (pc) (location-id location))
  (add-actor-to-location! location (pc))
  (cond
    [(Place? location)
     (notice (format "~a: Otava is now in ~a."
                     (format-timestamp (current-elapsed-time))
                     (get-location-name location)))]
    [else
     (cond
       [(and (not (= (route-traverse-time location) 1))
             (not (null? location-encounter-types))) ; TODO: "Trivial" route or something?
        (define name (route-shortname-from location old-location-id))
        (string-set! name 0 #\e)
        (notice (format "~a: Otava is now ~a" (format-timestamp (current-elapsed-time)) name))])])
  (location-on-enter! (current-location)))

(define (location-neighbors location)
  (cond
    [(route? location) (list (route-a location) (route-b location))]
    [(Place? location) (Place-routes location)]))

(define (get-current-location-choices)
  (define r
    (append (get-location-choices (current-location))
          (if (Place? (current-location)) (Place-choices (current-location)) '())
          (if (Place? (current-location)) (get-zone-choices (current-location)) '())
          (if (Place? (current-location)) (get-current-zone-choices) '())))
  r
  )

(define (location-has-item-of-id? location id)
  (define items (location-items items))
  (findf (λ (an-item)
           (cond
             [[symbol? an-item] (equal? an-item id)]
             [[item? an-item] (equal? (item-id an-item) id)]))
         items))

(define (Place-hidden-routes place)
  (filter (λ (route-id) (route-hidden? (get-route-by-id route-id))) (Place-routes place)))

(define (Place-get-perceptions place)
  (define unpruned-rows '())
  (case (location-id place)
    ['perimeter
     (when (and (pc-has-sense-organ? 'eyes)
                (or (equal? (get-current-light-level) 'bright)
                    (equal? (get-current-light-level) 'dark)))
       (append-element! unpruned-rows (tr "location type" "badlands"))
       (append-element! unpruned-rows (tr "perceived with eyes" "mutated, bonelike scraggly trees"))
       (when (and (current-zone) (eq? (Zone-name (current-zone)) "Abandoned car"))
        (append-element! unpruned-rows (tr "perceived with eyes" "decaying human body")))

       (for ([z (location-zones place)])
         (cond
           [(Clue? (Zone-clue? z))
            (cond
              [(pc-has-sense-organ? (SenseOrgan-id (Clue-requires (Zone-clue? z)))
                                    (SenseOrgan-level (Clue-requires (Zone-clue? z))))

               (append-element! unpruned-rows
                                (tr (format "perceived with ~a (lv ~a)"
                                            (SenseOrgan-id (Clue-requires (Zone-clue? z)))
                                            (SenseOrgan-level (Clue-requires (Zone-clue? z))))
                                    (Clue-description (Zone-clue? z))))])]))

       (when (Place? place)
         (for ([route-id (Place-routes place)])
           (define route (get-route-by-id route-id))
           (when (not (route-hidden? route))
             (append-element! unpruned-rows
                              (tr (format "known route")
                                  (format "~a" (get-traverse-text route (current-location)))))))))])
  unpruned-rows)

(define (Place-remove-choice! place target-choice-id)
  (set-Place-choices! place
                      (filter (λ (c) (not (equal? target-choice-id (choice-id c))))
                              (Place-choices place))))

(define (Place-add-choice! place choice)
  (set-Place-choices! place (append (Place-choices place) (list choice))))

; TODO: figure out where this belongs
(define (clue #:requires requires #:description description)
  (Clue* requires description #f))

(define (zone #:interactibles interactibles
              #:description description
              #:name [name ""]
              #:clue [clue '()])
  (Zone* interactibles #f clue #f description name))

(define (current-zone)
  (define location (current-location))
  (findf (λ (z) (Zone-pc-here? z)) (location-zones location)))

(define (current-zone-items location)
  (define items '())
  (when (not (eq? location #f))
    (when (not (eq? (current-zone) #f))
      (for ([interactible (Zone-interactibles (current-zone))])
        (cond
          [(item? interactible) (append-element! items interactible)]))))
  items)

(define (zone-features zone)
  (define interactibles '())
  (when (not (null? (current-zone)))
    (for ([interactible (Zone-interactibles (current-zone))])
      (cond
        [(symbol? interactible) ; TODO: Feature
        (append-element! interactibles interactible)]))
    )
  interactibles)

(define (zone-has-feature? zone feature)
  (if (not (equal? (member feature (zone-features zone)) #f)) #t #f))

(define (current-zone-has-feature? feature)
  (zone-has-feature? (current-zone) feature))

(define (find-item-in-current-zone id)
  (define items (current-zone-items (current-location)))
  (findf (λ (a) (if (item? a) (equal? (item-id a) id) (equal? a id))) items))

(define (add-interactible-to-zone! zone interactible)
  (set-Zone-interactibles! zone (cons interactible (Zone-interactibles zone))))

(define (remove-interactible-from-zone! zone interactible)
  (set-Zone-interactibles! zone (remove interactible (Zone-interactibles zone))))

(define (remove-interactible-from-current-zone! interactible)
  (remove-interactible-from-zone! (current-zone) interactible))

(define (get-current-zone-choices)
  (define duration 80)
  (define encounters? (if (location-encounter-types (current-location))
                          #t
                          #f))
  (if (and (current-zone) (eq? (Zone-name (current-zone)) "a brook"))
    (make-choice 'fish
                  (format "fish [~a ι]" duration)
                  (λ ()
                    (advance-time-until-next-interesting-event! duration encounters?)
                    (define roll-result (check "2d6" #:title "fishing roll" #:target-number 8 #:bonus '()))
                    (match roll-result
                      [(or 'critical-failure
                           'serious-failure
                           'failure)
                       (p "Otava catches no fish.")]
                      [(or 'narrow-success
                           'success
                           'critical-success)
                       (p "Otava waits a bit. A fat salmon swims upriver. Otava catches it.")
                       (add-item! (make-item 'salmon))]
                    )
                    (wait-for-confirm)
                    '())
                  )
    '()))

; poor name: this is "choices that lead to zones"
; ie., -> "clue-choices" + "known zones"
(define (get-zone-choices location)
  (define zone-choices '())
  (for ([z (location-zones location)])
    (define encounters? #t)
    (when (not (Zone-pc-here? z))
      (cond
        [(Clue? (Zone-clue? z))
         (cond
           [(pc-has-sense-organ? (SenseOrgan-id (Clue-requires (Zone-clue? z)))
                                 (SenseOrgan-level (Clue-requires (Zone-clue? z))))
            (define duration 10)
            (when (actor-has-condition-of-type? (pc) 'ankle-broken)
              (set! duration (* 3 duration)))
            (append-element!
             zone-choices
             (make-choice 'resolve-clue
                          (format "follow: ~a [~a ι]" (Clue-description (Zone-clue? z)) duration)
                          (λ ()
                            (advance-time-until-next-interesting-event! duration encounters?)
                            (for ([z_ (location-zones (current-location))])
                              (set-Zone-pc-here?! z_ #f))
                            (set-Zone-found?! z #t)
                            (set-Zone-clue?! z '())
                            (set-Zone-pc-here?! z #t)
                            '())))]
           [else
            (append-element! zone-choices
                             (make-unavailable-choice
                              "Unknown clue"
                              (format "requires: ~a lv ~a"
                                      (SenseOrgan-id (Clue-requires (Zone-clue? z)))
                                      (SenseOrgan-level (Clue-requires (Zone-clue? z))))))])]
        [else ; empty clue
         (cond
           [(Zone-found? z)
            (append-element!
             zone-choices
             (make-choice 'resolve-zone
                          (format "Go to: ~a" (Zone-name z))
                          (λ ()
                            (define iotas 5)
                            (advance-time-until-next-interesting-event! iotas encounters?)
                            (for ([z_ (location-zones (current-location))])
                              (set-Zone-pc-here?! z_ #f))
                            (set-Zone-found?! z #t)
                            (set-Zone-clue?! z '())
                            (set-Zone-pc-here?! z #t)
                            '())))]
           ; empty clue, but zone not found either
           ; -> hidden zone (think about this)
           [else '()])])))
  zone-choices)
