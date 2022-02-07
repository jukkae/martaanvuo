#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(lazy-require
  ["../actions/action.rkt"
    (action-details)]
  ["../combat/combat.rkt"
    (begin-combat!)])

(require
  "../actors/actor.rkt"
  "../core/api.rkt"
  "../enemies/encounters.rkt")

(require "0-types/place.rkt"
         "routes.rkt")

(define (location-on-enter! location)
  (dev-note "location-on-enter! tbd for location")
  (displayln location))


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
             (when (location-has-feature? location 'waiting-room-begin)
               (make-decision
                #:title "Enter waiting room."
                #:next-fragment 'waiting-room-begin
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
     ; spawn-blindscraper-encounter!
     ; spawn-grabberkin-encounter!
     spawn-grabberkin-and-blindscraper-encounter!
     ; spawn-two-blindscrapers-encounter!
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
  (format "~a~a" name features-str)
  )


(define (move-pc-to-location! location)
  ; TODO: location on-exit / on-enter triggers here
  (remove-actor-from-its-current-location! (pc))
  (set-actor-location! (pc) location)
  (add-actor-to-location! location (pc)))


(define (location-neighbors location)
  (cond ((route? location)
         (list
          (route-a location)
          (route-b location)))
        ((place? location)
         (place-routes location))))
