#lang at-exp racket


(require racket/lazy-require)

(require
  "time.rkt"

  "../../1-index/state.rkt"
  "../../1-index/content.rkt"

  "../../2-core/core.rkt"

  "../../3-types/actor.rkt"
  "../../3-types/location.rkt"
  "../../3-types/route.rkt"
  "../../3-types/world.rkt"
  )


; API
(define (get-current-actors)
  (define actors (location-actors (current-location)))
  actors)

(provide get-actor)
(define (get-actor id)
  (define actors (get-current-actors))
  (findf (λ (a) (eq? (actor-id a) id))
         actors)
  )

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
  (let* ([time (world-elapsed-time (current-world))]
         [time-today (remainder time day-length)])
    (- day-length time-today)))

(provide time-until-next-time-of-day)
(define (time-until-next-time-of-day)
  (- 100 (remainder (world-elapsed-time (current-world)) 100)))


; Uniqueness constraints(?), unidirectional paths(?), yada yada
(provide make-route-between)
(define (make-route-between
         id-a
         id-b
         traverse-time
         #:hidden? [hidden? #f]
         #:no-encounters? [no-encounters? #f] ; TODO: this is redundant, combine with types
         #:encounter-types [encounter-types '()]
         #:one-directional? [one-directional? #f]
         #:details [details '()])

  (when hidden? (error "Implement hidden paths"))
  (when no-encounters? (set! details (append-element details 'no-encounters)))

  (make-route
    id-a
    id-b
    traverse-time
    #:details details
    #:encounter-types encounter-types))

(provide get-route-by-id)
(define (get-route-by-id id)
  (define w (current-world))
  (define routes (world-routes w))
  (define r (findf (λ (route) (eq? id (location-id route)))
                   routes))
  (if r r '()))

(provide get-place-by-id)
(define (get-place-by-id id)
  (define w (current-world))
  (define places (world-places w))
  (define r (findf (λ (place) (eq? (location-id place) id))
                   places))
  (if r r '()))

(provide get-location-by-id)
(define (get-location-by-id id)
  (cond ((not (null? (get-place-by-id id)))
         (get-place-by-id id))
        ((not (null? (get-route-by-id id)))
         (get-route-by-id id))
        (else '())))


(define *number-of-places* 0)

(provide make-place)
(define (make-place
         #:id [id *number-of-places*]
         #:type [type '()]
         #:details [details '()]
         #:actors [actors '()]
         #:items [items '()]
         #:features [features '()]
         #:tags [tags '()]
         #:encounter-types [encounter-types '()]
         #:choices [choices '()]
         #:routes [routes '()]
         #:visited? [visited? #f]
         #:shortname [shortname ""])

  (set! *number-of-places* (add1 *number-of-places*))

  (define id-symbol
    (match id
     [(? symbol? id) id]
     [Natural (string->symbol (format "place-~a" id))]
     ))

  (place* id-symbol
          type
          details
          actors
          items
          features
          tags
          encounter-types
          routes
          visited?
          choices
          shortname))
