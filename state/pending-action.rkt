#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "../action.rkt"
         "../location.rkt"
         "../place.rkt"
         "../route.rkt")

(lazy-require
 ["state.rkt" (current-pending-action)])


(define (reset-pending-action!)
  (current-pending-action '()))

(define (set-pending-action! action)
  (current-pending-action action))

(define (get-pending-traverse-direction)
  (define pending-action (current-pending-action))
  (cond 
        ((eq? (action-symbol pending-action) 'traverse)
         (define target (action-target pending-action))
         
         (define details (action-details pending-action))
         
         (define direction
           (if (memq 'a-to-b details)
               'a-to-b
               'b-to-a))

         direction)
        ))

(define (get-continue-pending-action-name)
  (define pending-action (current-pending-action))
  (cond ((eq? (action-symbol pending-action) 'go-to-location)
         (string-append
          "[continue] Continue towards "
          (place-shortname (action-target pending-action))
          "."))
        ((eq? (action-symbol pending-action) 'traverse)
         (define target (action-target pending-action))
         
         (define details (action-details pending-action))
         
         (define direction
           (if (memq 'a-to-b details)
               'a-to-b
               'b-to-a))

         (define endpoint
           (case direction
             ['a-to-b (route-b target)]
             ['b-to-a (route-a target)]))
         
         (string-append
          "[continue] Continue towards "
          (place-shortname endpoint)
          "."))
        ((eq? (action-symbol pending-action) 'search-for-paths)
         (string-append
          "[continue] Search for paths."))
        (else (string-append "[continue] unknown action symbol: " (symbol->string (action-symbol pending-action))))))

(define (get-cancel-pending-action-and-go-back-name
         route
         pending-action)
  ; this assumes that pending-action is 'traverse, which might not be the case
  ;(define end-location (action-target pending-action))
  ; not very robust... anyhow, cancel direction is opposite to the pending action direction
  (define cancel-traverse-direction
    (if (memq 'b-to-a (action-details pending-action))
        'a-to-b
        'b-to-a))

  (define cancel-traverse-endpoint
    (case cancel-traverse-direction
      ['a-to-b (route-b route)]
      ['b-to-a (route-a route)]))
  
  (string-append "Go back to " (place-shortname cancel-traverse-endpoint) "."))

(define (get-cancel-and-go-back-destination
         route
         pending-action)
  (define cancel-traverse-direction
    (if (memq 'b-to-a (action-details pending-action))
        'a-to-b
        'b-to-a))

  (define cancel-traverse-endpoint
    (case cancel-traverse-direction
      ['a-to-b (route-b route)]
      ['b-to-a (route-a route)]))
  cancel-traverse-endpoint)