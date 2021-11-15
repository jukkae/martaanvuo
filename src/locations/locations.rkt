#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "location.rkt"
         "place.rkt"
         "route.rkt"
         "routes.rkt")

(lazy-require
 ["../action.rkt"
  (action-details)])


#;(lazy-require
   ["state/state.rkt"
    (get-pending-traverse-direction)])

(lazy-require
 ["../state/combat.rkt"
  (begin-combat!)])

(require "../api.rkt"
         "../actor.rkt")

(require "../enemies/blindscraper.rkt"
         "../enemies/grabberkin.rkt")

(require "../stance.rkt"
         "../status.rkt")

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
           'grabberkin-and-blindscraper)))

  (case encounter-type
    ['grabberkin
     (spawn-grabberkin-encounter!)
     ]
    ['blindscraper
     (spawn-blindscraper-encounter!)
     ]
    ['two-blindscrapers
     (spawn-two-blindscrapers-encounter!)
     ]
    ['grabberkin-and-blindscraper
     (spawn-grabberkin-and-blindscraper-encounter!)
     ]
    ))

(define (spawn-grabberkin-and-blindscraper-encounter!)
  ; grabberkin
  (begin-combat!)
  (p "Something grabs Otava by the ankle and pulls. She staggers, barely manages to stay upright, and immediately goes for her bolt cutters.") ; could cause fall-down on failed roll

  (define hp 11)

  (define e1 (make-actor "Grabberkin" hp))
  (set-actor-dexterity! e1 4)
  (set-actor-strength! e1 11)
  (set-trait! e1 "defense" -1)
  (set-trait! e1 "melee-attack-skill" 1)
  (set-trait! e1 "hp-hidden" #f)
  (move-actor-to-location! e1 (current-location))

  (inflict-status! (pc) (status 'bound 10))
         
  (set-actor-stance! e1 (stance "α" 'engaged "grabbing Otava's ankle"))
  

  (define i 1)
  (define enemy (make-actor "Blindscraper" 3))
  (set-actor-dexterity! enemy 13)
  (set-trait! enemy "defense" 1)
  (set-trait! enemy "melee-attack-skill" 1)
  (set-trait! enemy "size" "small")
  (move-actor-to-location! enemy (current-location))

  (define sign
    (case i
      [(0) "α"]
      [(1) "β"]
      [(2) "γ"]
      [(3) "δ"]
      [else ""]))
  
  (define range
    (if (= i 0)
        'close
        'mid))
  (define description
    (case i
      [(0) "right"]
      [(1) "left"]
      [else "right"]))
  (define enemy-stance
    (stance sign range description))
           
  (set-actor-stance! enemy enemy-stance)
  
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
  (string-append name
                 features-str)
  )


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




