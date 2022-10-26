#lang at-exp racket

(require racket/lazy-require)

(require "time.rkt"

         "../../1-index/state.rkt"
         "../../1-index/content.rkt"

         "../../2-core/core.rkt"

         "../../3-types/actor.rkt"
         "../../3-types/location.rkt"
         "../../3-types/place.rkt"
         "../../3-types/route.rkt"
         "../../3-types/world.rkt")

(define (get-current-actors)
  (define actors (location-actors (current-location)))
  actors)

(provide get-actor)
(define (get-actor id)
  (define actors (get-current-actors))
  (findf (λ (a) (equal? (actor-id a) id)) actors))

(define (get-current-time-of-day)
  (time-of-day-from-iotas (current-elapsed-time)))

(provide get-current-light-level)
(define (get-current-light-level)
  (case (location-light-level (current-location))
    ['natural
     (case (get-current-time-of-day)
       ['morning 'bright]
       ['midday 'bright]
       ['afternoon 'bright]
       ['evening 'dark]
       ['night 'pitch-black])]
    [else (location-light-level (current-location))]))

(provide get-current-noise-level)
(define (get-current-noise-level)
  (cond
    [(and (or (equal? (location-id (current-location)) 'tunnels-3)
              (equal? (location-id (current-location)) 'tunnels-4)
              (equal? (location-id (current-location))
                      'the-maw)) ; TODO: FIXME: proper "neighboring locations" type of thing
          (location-has-feature? (get-location-by-id 'reactor-room) 'running-centrifuge))
     'moderately-noisy]
    [(location-has-feature? (current-location) 'running-centrifuge) 'noisy]
    [(equal? (location-type (current-location)) 'ext) 'quiet]
    [else 'silent]))

; world-as-simulation / scripting API
(provide remove-actor-from-its-current-location!)
(define (remove-actor-from-its-current-location! actor)
  (define current-loc (get-location-by-id (actor-location-id actor)))

  (when (not (null? current-loc))
    (remove-actor-from-location! current-loc actor))
  (set-actor-location-id! actor '()))

; world-as-simulation / scripting API
(provide move-actor-to-location!)
(define (move-actor-to-location! actor location)
  (remove-actor-from-its-current-location! actor)
  (set-actor-location-id! actor (location-id location))
  (add-actor-to-location! location actor))

; time helpers
(provide time-until-next-morning)
(define (time-until-next-morning)
  (let* ([time (world-elapsed-time (current-world))] [time-today (remainder time day-length)])
    (- day-length time-today)))

(provide time-until-next-time-of-day)
(define (time-until-next-time-of-day)
  (- 100 (remainder (world-elapsed-time (current-world)) 100)))

; Uniqueness constraints(?)
(provide route-between)
(define (route-between id-a
                       id-b
                       traverse-time
                       type
                       #:hidden? [hidden? #f]
                       #:light-level [light-level 'natural]
                       #:encounter-types [encounter-types '()]
                       #:one-directional? [one-directional? #f]
                       #:details [details '()]
                       #:descr-from-a [descr-from-a '()]
                       #:descr-from-b [descr-from-b '()]
                       #:tags [tags '()])

  (make-route id-a
              id-b
              traverse-time
              one-directional?
              hidden?
              #:details details
              #:type type
              #:light-level light-level
              #:encounter-types encounter-types
              #:descr-from-a descr-from-a
              #:descr-from-b descr-from-b
              #:tags tags))

(provide add-route-between!)
(define (add-route-between! id-a
                            id-b
                            traverse-time
                            type
                            #:hidden? [hidden? #f]
                            #:light-level [light-level 'natural]
                            #:encounter-types [encounter-types '()]
                            #:one-directional? [one-directional? #f]
                            #:details [details '()])

  (define route
    (route-between id-a
                   id-b
                   traverse-time
                   type
                   #:hidden? hidden?
                   #:light-level light-level
                   #:encounter-types encounter-types
                   #:one-directional? one-directional?
                   #:details details))

  (define route-id (location-id route))

  (define place-a (get-place-by-id id-a))
  (define place-b (get-place-by-id id-b))
  (set-Place-routes! place-a (append-element (Place-routes place-a) route-id))
  (when (not (route-one-directional? route))
    (set-Place-routes! place-b (append-element (Place-routes place-b) route-id)))

  (set-world-routes! (current-world) (append-element (world-routes (current-world)) route)))

(provide get-route-by-id)
(define (get-route-by-id id)
  (define w (current-world))
  (define routes (world-routes w))
  (define r (findf (λ (route) (equal? id (location-id route))) routes))
  (if r r '()))

(provide get-route-between)
(define (get-route-between a b)
  (define w (current-world))
  (define routes (world-routes w))
  (define r (findf (λ (route)
                     (or (and (equal? a (route-a route))
                              (equal? b (route-b route)))
                         (and (equal? b (route-a route))
                              (equal? a (route-b route)))))
                   routes))
  (if r r '()))

(provide get-place-by-id)
(define (get-place-by-id id)
  (define w (current-world))
  (define places (world-places w))
  (define r (findf (λ (place) (equal? (location-id place) id)) places))
  (if r r '()))

(provide get-location-by-id)
(define (get-location-by-id id)
  (cond
    [(not (null? (get-place-by-id id))) (get-place-by-id id)]
    [(not (null? (get-route-by-id id))) (get-route-by-id id)]
    [else '()]))

(define *number-of-places* 0)

(provide place)
(define (place #:id [id *number-of-places*]
               #:type [type '()]
               #:size [size '()]
               #:details [details '()]
               #:actors [actors '()]
               #:items [items '()]
               #:features [features '()]
               #:hidden-features [hidden-features '()]
               #:zones [zones '()]
               #:tags [tags '()]
               #:light-level [light-level 'natural]
               #:encounter-types [encounter-types '()]
               #:choices [choices '()]
               #:routes [routes '()]
               #:visited? [visited? #f]
               #:explored [explored 'not-explored]
               #:on-explore-rules! [on-explore-rules! '()]
               #:shortname [shortname ""])

  (set! *number-of-places* (add1 *number-of-places*))

  (define id-symbol
    (match id
      [(? symbol? id) id]
      [Natural (string->symbol (format "place-~a" id))]))

  (Place* id-symbol
          type
          size
          details
          actors
          items
          features
          hidden-features
          zones
          tags
          light-level
          encounter-types
          routes
          visited?
          explored
          on-explore-rules!
          choices
          shortname))

(provide connect-places-and-routes!)
(define (connect-places-and-routes! places routes)
  (define (find-place Place-id)
    (findf (λ (place) (equal? (location-id place) Place-id)) places))
  (for ([route routes])
    (define route-id (location-id route))
    (define id-a (route-a route))
    (define id-b (route-b route))
    (define place-a (find-place id-a))
    (define place-b (find-place id-b))
    (when (not place-a)
      (dev-note (format "Null: ~a" id-a)))
    (when (not place-b)
      (dev-note (format "Null: ~a" id-b)))
    (set-Place-routes! place-a (append-element (Place-routes place-a) route-id))
    (when (not (route-one-directional? route))
      (set-Place-routes! place-b (append-element (Place-routes place-b) route-id)))))

(provide add-place!)
(define (add-place! place)
  (set-world-places! (current-world) (append-element (world-places (current-world)) place)))
