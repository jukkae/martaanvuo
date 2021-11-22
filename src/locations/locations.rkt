#lang racket

(provide (all-defined-out))

(require racket/lazy-require)


(lazy-require
  ["../actions/action.rkt"
    (action-details)]
  ["../state/combat.rkt"
    (begin-combat!)])

(require 
  "../actors/actor.rkt"
  "../core/api.rkt"
  "../enemies/encounters.rkt")

(require "place.rkt"
         "routes.rkt")

(define (location-on-enter! location)
  (dev-note "location-on-enter! tbd for location")
  (displayln location))


(define (get-location-decisions location)
  (condense (list

             ; definition / content goes to -> features, or world, or something
             (when (location-has-feature? location 'stiltman)
               (define manuscript-quest (quest-exists? 'anthead-monograph))
               (cond ((not manuscript-quest)
                      (make-decision
                       #:title "Talk to the stilted figure."
                       #:on-resolve! (thunk
                                      (p "Otava goes closer to the figure flailing peculiarly above water. It turns out to be a man, balancing precariously on an insectlike, three-legged contraption of rods and springs and wire."))
                       #:next-fragment 'begin-stiltman-dialogue
                       ))
                     (else
                      (make-decision
                       #:title "Talk to Stiltman."
                       #:on-resolve! (thunk
                                      (p "Stiltman flickers and flails above water, and Otava shouts out to him."))
                       #:next-fragment 'stiltman-continue-dialogue
                       ))))

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
     spawn-grabberkin-encounter!
     ; spawn-grabberkin-and-blindscraper-encounter!
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
