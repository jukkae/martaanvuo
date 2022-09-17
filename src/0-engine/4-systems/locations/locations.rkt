#lang at-exp racket

(provide (all-defined-out))
(provide
  (all-from-out
    "routes.rkt"
    ))

(require
  racket/lazy-require
  "../../3-types/action.rkt"
  "../../3-types/item.rkt"
  )

(lazy-require ["../world/world.rkt"
  (remove-actor-from-its-current-location!
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
             (when (location-has-feature? location 'martaanvuo-book)
               (make-decision
                #:title "Pick up the book."
                #:next-fragment (thunk
                                 (p "Otava takes the book in her hands. Bound in supple leather, heavier than you'd expect. The book fills her with apprehension and dread.")
                                 'read-the-book
                                 )
                ))
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

(define (get-location-short-description location)
  (cond [(Place? location)
         (if (equal? (Place-shortname location) "")
             (capitalize-first-letter (string-replace (~a (location-id location)) "-" " "))
             (Place-shortname location))]
        [(route? location)
         (route-shortname (location-id location))]))


(define (move-pc-to-location! location)
  (reset-pending-action!)
  (remove-actor-from-its-current-location! (pc))
  (set-actor-location-id! (pc) (location-id location))
  (add-actor-to-location! location (pc))
  (notice (format "~a ι: Otava is now in ~a." (current-elapsed-time) (get-location-short-description location)))
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
    ))

(define (location-has-item-of-id? location id)
  (define items (location-items items))
  (findf (λ (an-item)
           (cond ([symbol? an-item] (equal? an-item id))
                 ([item? an-item] (equal? (item-id an-item) id))
              ))
         items))
