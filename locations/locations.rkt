#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "location.rkt"
         "place.rkt"
         "route.rkt")

(lazy-require
 ["../action.rkt"
  (action-details)])


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