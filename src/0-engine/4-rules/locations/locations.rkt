#lang at-exp racket

(provide (all-defined-out))
(provide
  (all-from-out
    "routes.rkt"
    ))

(require racket/lazy-require)

(lazy-require ["../world/world.rkt"
  (remove-actor-from-its-current-location!
   )])
(lazy-require ["../../3-types/action.rkt"
  (action-details
   )])
(lazy-require ["../../6-combat/combat.rkt"
  (begin-combat!
   )])
(lazy-require ["../../7-state/state/mutators.rkt"
  (current-location
   pc
   )])
(lazy-require ["../../7-state/state/pending-action.rkt"
  (reset-pending-action!
  )])

(require
  "routes.rkt"
  "../actors/actor.rkt"
  "../enemies/encounters.rkt"
  "../fragments/decision.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"
  "../../3-types/location-ids.rkt"
  "../../3-types/location.rkt"
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
                #:on-resolve! (thunk
                               (p "Otava takes the book in her hands. Bound in supple leather, heavier than you'd expect. The book fills her with apprehension and dread."))
                #:next-fragment 'read-the-book
                ))
             (when (location-has-feature? location 'martaanvuo-console)
               (make-decision
                #:title "Turn on the terminal."
                #:on-resolve! (thunk
                               (p "Otava turns on the terminal. It clicks and whirrs, then the display comes to life."))
                #:next-fragment 'turn-on-martaanvuo-terminal
                ))
             )))

(define (spawn-enemies)
  (define encounter-types
    (list
    ;;;  spawn-blindscraper-encounter!
     spawn-grabberkin-encounter!
    ;;;  spawn-grabberkin-and-blindscraper-encounter!
    ;;;  spawn-two-blindscrapers-encounter!
    ))

  (cond ((place? (current-location))
          (cond ((eq? (location-type (current-location)) 'ridges)
                 (spawn-blindscraper-encounter!))
                ((eq? (location-type (current-location)) 'valleys)
                 (spawn-grabberkin-encounter!))
                (else ((take-random encounter-types)))))
        ((route? (current-location))
          ((take-random encounter-types))))
  )

(define (get-location-short-description location)
  (cond [(place? location)
         (if (eq? (place-shortname location) "")
             (capitalize-first-letter (string-replace (~a (location-id location)) "-" " "))
             (place-shortname location))]
        [(route? location)
         (route-shortname (location-id location))]))


(define (move-pc-to-location! location)
  (reset-pending-action!)
  (remove-actor-from-its-current-location! (pc))
  (set-actor-location-id! (pc) (location-id location))
  (add-actor-to-location! location (pc)))


(define (location-neighbors location)
  (cond ((route? location)
         (list
          (route-a location)
          (route-b location)))
        ((place? location)
         (place-routes location))))