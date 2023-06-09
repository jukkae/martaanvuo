#lang at-exp racket

(provide (all-defined-out))

(require "../1-index/state.rkt"

         "../3-types/action.rkt"
         "../3-types/location.rkt"
         "../3-types/place.rkt"
         "../3-types/route.rkt"

         "../4-systems/world/world.rkt")

(define (reset-pending-action!)
  (current-pending-action '()))

; Assumes that pending action exists
(define (get-pending-traverse-direction)
  (define pending-action (current-pending-action))
  (cond
    [(or (equal? (action-symbol pending-action) 'traverse)
         (equal? (action-symbol pending-action) 'cancel-traverse))
     (define target (action-target pending-action))

     (define details (action-details pending-action))

     (define direction (if (memq 'a-to-b details) 'a-to-b 'b-to-a))

     direction]))

(define (get-continue-pending-action-name)
  (define pending-action (current-pending-action))
  (format "~a [~a ι]"
          (case (action-symbol pending-action)
            ['go-to-location
             (define target (action-target pending-action))
             (when (symbol? target)
               (set! target (get-location-by-id target)))
             (format "[continue] Continue towards ~a." (Place-shortname target))]
            ['go-to-location
             (define target (action-target pending-action))
             (when (symbol? target)
               (set! target (get-location-by-id target)))
             (format "[continue] Continue towards ~a." (Place-shortname target))]
            ['traverse
             (define target (action-target pending-action))
             (when (symbol? target)
               (set! target (get-location-by-id target)))

             (define details (action-details pending-action))

             (define direction (if (memq 'a-to-b details) 'a-to-b 'b-to-a))

             (define endpoint
               (get-location-by-id (case direction
                                     ['a-to-b (route-b target)]
                                     ['b-to-a (route-a target)])))

             (define known? (route-fully-known? (current-location)))

             (format "[continue] Continue towards ~a." (if known? (Place-shortname endpoint) "???"))]
            ['explore "[continue] explore"]
            ['cancel-traverse
             (define target (action-target pending-action))
             (format "[continue] back to ~a" target)]
            [else (format "[continue] unknown action symbol: ~a" (action-symbol pending-action))])
          (action-duration pending-action)))

(define (get-cancel-pending-action-and-go-back-name route pending-action)
  ; this assumes that pending-action is 'traverse, which might not be the case
  ;(define end-location (action-target pending-action))
  ; not very robust... anyhow, cancel direction is opposite to the pending action direction
  (define cancel-traverse-direction
    (if (memq 'b-to-a (action-details pending-action)) 'a-to-b 'b-to-a))

  (define cancel-traverse-endpoint-id
    (case cancel-traverse-direction
      ['a-to-b (route-b route)]
      ['b-to-a (route-a route)]))

  (define target (get-location-by-id cancel-traverse-endpoint-id))

  (format "[cancel] Go back to ~a." (Place-shortname target)))

(define (get-cancel-and-go-back-destination route pending-action)
  (define cancel-traverse-direction
    (if (memq 'b-to-a (action-details pending-action)) 'a-to-b 'b-to-a))

  (define cancel-traverse-endpoint
    (case cancel-traverse-direction
      ['a-to-b (route-b route)]
      ['b-to-a (route-a route)]))
  cancel-traverse-endpoint)
