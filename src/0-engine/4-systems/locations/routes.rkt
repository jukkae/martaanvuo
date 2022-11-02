#lang at-exp racket

(provide route-other-end-from
         route-shortname
         route-shortname-from
         route-description-from
         set-route-endpoint-visited!)

(require racket/lazy-require

         "../world/world.rkt"

         "../../2-core/dev-note.rkt"
         "../../3-types/location.rkt"
         "../../3-types/place.rkt"
         "../../3-types/route.rkt"
         "../../7-state/pending-action.rkt")

(lazy-require ["../../7-state/state.rkt" (current-pending-action)])


; returns id
(define (route-other-end-from route-or-id startpoint-id)
  (define route
    (cond
      [(route? route-or-id) route-or-id]
      [else (get-location-by-id route-or-id)]))
  (when (location? startpoint-id)
    (set! startpoint-id (location-id startpoint-id)))
  (define start
    (cond
      [(equal? startpoint-id (route-a route)) 'a]
      [(equal? startpoint-id (route-b route)) 'b]))
  (define endpoint-id
    (case start
      ['a (route-b route)]
      ['b (route-a route)]))

  endpoint-id)

(define (set-route-endpoint-visited! route-or-id endpoint-id)
  (define route
    (cond
      [(route? route-or-id) route-or-id]
      [else (get-location-by-id route-or-id)]))
  (define endpoint
    (cond
      [(equal? endpoint-id (route-a route)) 'a]
      [(equal? endpoint-id (route-b route)) 'b]))
  (case endpoint
    ['a (add-detail-to-location! route 'a-visited)]
    ['b (add-detail-to-location! route 'b-visited)]))

(define (route-place-known? route-or-id place)
  (define route
    (cond
      [(route? route-or-id) route-or-id]
      [else (get-location-by-id route-or-id)]))
  (define endpoint
    (cond
      [(equal? (location-id place) (route-a route)) 'a]
      [(equal? (location-id place) (route-b route)) 'b]))
  (case endpoint
    ['a (Place-visited? (route-a route))]
    ['b (Place-visited? (route-b route))]))

(define (route-description-from route-or-id startpoint)
  (when (null? startpoint)
    (dev-note (format "route-or-id: ~a" route-or-id))
    (error "route-description-from requires a non-null startpoint!"))
  (when (symbol? startpoint)
    (set! startpoint (get-location-by-id startpoint)))
  (define route
    (cond
      [(route? route-or-id) route-or-id]
      [else (get-location-by-id route-or-id)]))

  (define direction (if (equal? (route-a route) (location-id startpoint)) 'a-to-b 'b-to-a))

  (define endpoint
    (case direction
      ['a-to-b (route-b route)]
      ['b-to-a (route-a route)]))

  (when (symbol? startpoint)
    (set! startpoint (get-location-by-id startpoint)))
  (when (symbol? endpoint)
    (set! endpoint (get-location-by-id endpoint)))

  (format "~a to ~a"
          (cond
            [(equal? direction 'a-to-b)
             (if (not (null? (route-descr-from-a route))) (route-descr-from-a route) "")]
            [else (if (not (null? (route-descr-from-b route))) (route-descr-from-b route) "")])
          (cond
            [(or (route-fully-known? route) (location-has-tag? route 'known))
             (format "~a" (Place-shortname endpoint))]
            [else "???"])))

(define (route-shortname-from route-or-id startpoint)
  (when (null? startpoint)
    (dev-note (format "route-or-id: ~a" route-or-id))
    (error "route-shortname-from requires a non-null startpoint!"))
  (define route
    (cond
      [(route? route-or-id) route-or-id]
      [else (get-location-by-id route-or-id)]))

  (define direction (if (equal? (route-a route) startpoint) 'a-to-b 'b-to-a))

  (define endpoint
    (case direction
      ['a-to-b (route-b route)]
      ['b-to-a (route-a route)]))

  (when (symbol? startpoint)
    (set! startpoint (get-location-by-id startpoint)))
  (when (symbol? endpoint)
    (set! endpoint (get-location-by-id endpoint)))

  (cond
    [(or (route-fully-known? route) (location-has-tag? route 'known))
     (format "En route: ~a – ~a" (Place-shortname startpoint) (Place-shortname endpoint))]
    [else
     (cond
       [(not (Place-visited? startpoint)) (format "En route: ??? – ~a" (Place-shortname endpoint))]
       [(not (Place-visited? endpoint)) (format "En route: ~a – ???" (Place-shortname startpoint))]
       [else "????"])]))

(define (route-shortname route-or-id)
  (define route
    (cond
      [(route? route-or-id) route-or-id]
      [else (get-location-by-id route-or-id)]))

  (define direction (get-pending-traverse-direction))

  (define startpoint
    (case direction
      ['a-to-b (route-a route)]
      ['b-to-a (route-b route)]))
  (define endpoint
    (case direction
      ['a-to-b (route-b route)]
      ['b-to-a (route-a route)]))

  (when (symbol? startpoint)
    (set! startpoint (get-location-by-id startpoint)))
  (when (symbol? endpoint)
    (set! endpoint (get-location-by-id endpoint)))

  (define startpoint-name (if (Place-visited? startpoint) (Place-shortname startpoint) "???"))

  (define endpoint-name
    (if (or (Place-visited? endpoint) (location-has-tag? route 'known))
        (Place-shortname endpoint)
        "???"))

  (format "En route: ~a – ~a" startpoint-name endpoint-name))
