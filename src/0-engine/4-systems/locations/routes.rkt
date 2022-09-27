#lang at-exp racket

(provide route-other-end-from
         route-shortname
         route-shortname-from
         set-route-endpoint-visited!)

(require
  "../world/world.rkt"

  "../../3-types/location.rkt"
  "../../3-types/place.rkt"
  "../../3-types/route.rkt"
  "../../7-state/pending-action.rkt"
  )

; returns id
(define (route-other-end-from route-or-id startpoint-id)
  (define route
    (cond [(route? route-or-id)
           route-or-id]
          [else (get-location-by-id route-or-id)]))
  (when (location? startpoint-id) (set! startpoint-id (location-id startpoint-id)))
  (define start
    (cond ((equal? startpoint-id
                (route-a route))
           'a)
          ((equal? startpoint-id
                (route-b route))
           'b)))
  (define endpoint-id
    (case start
      ['a (route-b route)]
      ['b (route-a route)]))

  endpoint-id)

(define (set-route-endpoint-visited! route-or-id endpoint-id)
  (define route
    (cond [(route? route-or-id)
           route-or-id]
          [else (get-location-by-id route-or-id)]))
  (define endpoint
    (cond ((equal? endpoint-id
                (route-a route))
           'a)
          ((equal? endpoint-id
                (route-b route))
           'b)))
  (case endpoint
    ['a (add-detail-to-location! route 'a-visited)]
    ['b (add-detail-to-location! route 'b-visited)]))


(define (route-place-known? route-or-id place)
  (define route
    (cond [(route? route-or-id)
           route-or-id]
          [else (get-location-by-id route-or-id)]))
  (define endpoint
    (cond ((equal? (location-id place)
                (route-a route))
           'a)
          ((equal? (location-id place)
                (route-b route))
           'b)))
  (case endpoint
    ['a (Place-visited? (route-a route))]
    ['b (Place-visited? (route-b route))])
  )

(define (route-shortname-from route-or-id startpoint)
  (define route
    (cond [(route? route-or-id)
           route-or-id]
          [else (get-location-by-id route-or-id)]))

  (define direction (if (equal? (route-a route) startpoint)
                        'a-to-b
                        'b-to-a))

  (define endpoint
    (case direction
      ['a-to-b (route-b route)]
      ['b-to-a (route-a route)]))

  (when (symbol? startpoint)
    (set! startpoint (get-location-by-id startpoint)))
  (when (symbol? endpoint)
    (set! endpoint (get-location-by-id endpoint)))

  (cond ((route-fully-known? route)
         (format "En route: ~a – ~a"
                 (Place-shortname startpoint)
                 (Place-shortname endpoint)))
        (else
         (format "En route: ~a – ???" (Place-shortname startpoint))))
  )

(define (route-shortname route-or-id)
  (define route
    (cond [(route? route-or-id)
           route-or-id]
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

  (cond ((route-fully-known? route)
         (format "En route: ~a – ~a"
                 (Place-shortname startpoint)
                 (Place-shortname endpoint)))
        (else
         (format "En route: ~a – ???" (Place-shortname startpoint)))))
