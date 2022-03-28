#lang at-exp racket

(provide route-other-end-from
         route-shortname
         set-route-endpoint-visited!)

(require "0-types/route.rkt"
         "../0-api/api.rkt")

; returns id
(define (route-other-end-from route-or-id startpoint-id)
  (define route
    (cond [(route? route-or-id)
           route-or-id]
          [else (get-location-by-id route-or-id)]))
  (when (location? startpoint-id) (set! startpoint-id (location-id startpoint-id)))
  (define start
    (cond ((eq? startpoint-id
                (route-a route))
           'a)
          ((eq? startpoint-id
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
    (cond ((eq? endpoint-id
                (route-a route))
           'a)
          ((eq? endpoint-id
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
    (cond ((eq? (location-id place)
                (route-a route))
           'a)
          ((eq? (location-id place)
                (route-b route))
           'b)))
  (case endpoint
    ['a (place-visited? (route-a route))]
    ['b (place-visited? (route-b route))])
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
                 (place-shortname startpoint)
                 (place-shortname endpoint)))
        (else
         (format "En route: ~a – ???" (place-shortname startpoint)))))
