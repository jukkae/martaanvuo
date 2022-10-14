#lang at-exp racket

(provide (all-defined-out))
(provide
  (all-from-out
    "routes.rkt"
    ))

(require
  racket/lazy-require
  "../pc/pc.rkt"
  "../world/time.rkt"
  "../../3-types/action.rkt"
  "../../3-types/choice.rkt"
  "../../3-types/clue.rkt"
  "../../3-types/sense-organ.rkt"
  "../../3-types/item.rkt"
  )

(lazy-require ["../world/world.rkt"
  (remove-actor-from-its-current-location!
   get-route-by-id
   )])
(lazy-require ["../../6-combat/combat.rkt"
  (begin-combat!
   )])
(lazy-require ["../../7-state/mutators.rkt"
  (current-location
   pc
   )])
(lazy-require ["../../7-state/pending-action.rkt"
  (reset-pending-action!
  )])

(lazy-require ["../../../1-content/world/locations/location-choices.rkt"
  (get-location-choices
  )])

(lazy-require ["../../../1-content/encounters/encounters.rkt"
  (spawn-human-fighter-encounter!
   spawn-grabberkin-encounter!
   spawn-blindscraper-encounter!
   spawn-two-blindscrapers-encounter!
   spawn-grabberkin-and-blindscraper-encounter!
   spawn-voidfloater-encounter!
   spawn-limbtearer-encounter!
   )])

(require
  "routes.rkt"
  "../actors/actor.rkt"

  "../../1-index/state.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"

  "../../3-types/decision.rkt"
  "../../3-types/location-ids.rkt"
  "../../3-types/location.rkt"
  "../../3-types/place.rkt"
  "../../3-types/route.rkt"
  "../../3-types/actor.rkt"
  )

(define (location-on-enter! location)
  (dev-note (format "location-on-enter! tbd for location ~a" location)))


(define (get-location-decisions location)
  (condense (list

             ; definition / content goes to -> features, or world, or something
             (when (location-has-feature? location 'martaanvuo-terminal)
               (make-decision
                #:title "Turn on the terminal."
                #:next-fragment (thunk
                                 (p "Otava turns on the terminal. It clicks and whirrs, then the display comes to life.")
                                 'turn-on-martaanvuo-terminal)
                ))
             )))

(define (spawn-enemies encounter-type)
  ; TODO: move to encounter-specific content
  (case encounter-type
    ['human-fighter (spawn-human-fighter-encounter!)]
    ['grabberkin (spawn-grabberkin-encounter!)]
    ['blindscraper (spawn-blindscraper-encounter!)]
    ['two-blindscrapers (spawn-two-blindscrapers-encounter!)]
    ['grabberkin-and-blindscraper (spawn-grabberkin-and-blindscraper-encounter!)]
    ['voidfloater (spawn-voidfloater-encounter!)]
    ['limbtearer (spawn-limbtearer-encounter!)]
    [else (dev-note (format "Unknown encounter type: ~a" encounter-type))])
    )

(define (spawn-encounter)
  (current-counters++ 'enemy-encounters)

  (when (not (null? (location-encounter-types (current-location))))
    (spawn-enemies (take-random (location-encounter-types (current-location))))))

(define (get-location-name location)
  (format "~a"
          (cond [(Place? location)
                 (if (equal? (Place-shortname location) "")
                     (capitalize-first-letter (string-replace (~a (location-id location)) "-" " "))
                     (Place-shortname location))]
                [(route? location)
                 (route-shortname (location-id location))])
          )
  )

(define (get-location-short-description location)
  (format "~a~a"
          (get-location-name location)
          (cond [(and (or (pc-has-sense-organ? 'eyes)
                          (pc-has-sense-organ? 'echolocation))
                      (not (null? (location-size (current-location)))))
                 (format " [~a space]" (location-size (current-location)))]
                [else ""]
                )
          )
  )

(define (move-pc-to-location! location)
  (reset-pending-action!)
  (define old-location-id
    (if (not (null? (current-location)))
        (location-id (current-location))
        '()))
  (remove-actor-from-its-current-location! (pc))
  (set-actor-location-id! (pc) (location-id location))
  (add-actor-to-location! location (pc))
  (cond [(Place? location)
         (notice (format "~a: Otava is now in ~a." (format-timestamp (current-elapsed-time)) (get-location-name location)))]
        [else
         (cond [(and (not (= (route-traverse-time location) 1))
                     (not (null? location-encounter-types))) ; TODO: "Trivial" route or something?
                (define name (route-shortname-from location old-location-id))
                (string-set! name 0 #\e)
                (notice (format "~a: Otava is now ~a" (format-timestamp (current-elapsed-time)) name))])
         ])
  )


(define (location-neighbors location)
  (cond ((route? location)
         (list
          (route-a location)
          (route-b location)))
        ((Place? location)
         (Place-routes location))))

(define (get-current-location-choices)
  (append
    (get-location-choices (current-location))
    (if (Place? (current-location))
      (Place-choices (current-location))
      '())
    (if (Place? (current-location))
      (get-clue-choices (current-location))
      '())
    (if (Place? (current-location))
      (get-zone-choices (current-location))
      '())
    )
    )

(define (location-has-item-of-id? location id)
  (define items (location-items items))
  (findf (λ (an-item)
           (cond ([symbol? an-item] (equal? an-item id))
                 ([item? an-item] (equal? (item-id an-item) id))
              ))
         items))

(define (Place-hidden-routes place)
  (filter (λ (route-id) (route-hidden? (get-route-by-id route-id)))
          (Place-routes place))
  )

(define (Place-get-perceptions place)
  (define unpruned-rows '())
  (for ([clue (Place-clues place)])
    (cond [(pc-has-sense-organ? (SenseOrgan-id (Clue-requires clue)) (SenseOrgan-level (Clue-requires clue)))
            (set! unpruned-rows
                  (append-element unpruned-rows
                                  (tr
                                  (format "~a [lv ~a]" (SenseOrgan-id (Clue-requires clue)) (SenseOrgan-level (Clue-requires clue)))
                                  (format "~a" (Clue-description clue)))))
           ]
          [else
           (set! unpruned-rows
                  (append-element unpruned-rows
                                  (tr
                                  (format "~a [lv ~a]" (SenseOrgan-id (Clue-requires clue)) (SenseOrgan-level (Clue-requires clue)))
                                  (format "[unknown - missing sense organ]"))))
           ])
            )
  unpruned-rows
  )


(define (Place-remove-choice! place target-choice-id)
  (set-Place-choices! place
    (filter (λ (c)
              (not (equal? target-choice-id
                      (choice-id c))))
            (Place-choices place)
            )))

(define (Place-add-choice! place choice)
  (set-Place-choices!
    place
    (append (Place-choices place) (list choice))
    ))


; TODO: figure out where this belongs
(define (clue
 #:requires requires
 #:description description
 #:on-resolve-rules [on-resolve-rules '()]
 )
 (Clue*
  requires
  description
  on-resolve-rules
  #f
  ))

(define (get-clue-choices place)
'()
;  (define clues (Place-clues place))
;  (for/list ([clue clues])
;   (define with-discard (append (Clue-resolution-rules clue)
;                                `(
;                                 ;  ,(set-Clue-resolved! clue #t)
;                                  '()
;                                  )))
; (cond [(pc-has-sense-organ? (SenseOrgan-id (Clue-requires clue)))
;        (make-choice
;         'resolve-clue
;         (format "Resolve clue: ~a" (Clue-description clue))
;         with-discard
;         )
;        ])
;   )
  )

(define (zone
         #:interactibles interactibles
         #:clue [clue '()])
  (Zone* interactibles #f clue #f))

(define (get-zone-choices location)
 (for/list ([z (location-zones location)])
;  (displayln z)
  (define choice-title
    (cond
        [(Clue? (Zone-clue? z))
         (cond [(pc-has-sense-organ?
                  (SenseOrgan-id (Clue-requires (Zone-clue? z)))
                  (SenseOrgan-level (Clue-requires (Zone-clue? z))))
                (format "Resolve clue: ~a" (Clue-description (Zone-clue? z))
                )
               ]
               [else (format "N/A [requires: ~a lv ~a]"
                      (SenseOrgan-id (Clue-requires (Zone-clue? z)))
                      (SenseOrgan-level (Clue-requires (Zone-clue? z))))])]
        [else
          (format "~a" "[empty clue]")
        ]
        ))
  (make-choice
   'resolve-zone
   choice-title
   (λ ()
    (set-Zone-found?! z #t)
    ; (dev-note "REMOVING ZONE")
    ; (set-location-zones!
    ;   location
    ;   (remove zone (location-zones location)))
    '())))
 )
