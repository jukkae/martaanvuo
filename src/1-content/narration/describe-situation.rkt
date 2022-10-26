#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "../../0-engine/0-api/api.rkt")

(lazy-require ["combat-narration.rkt" (describe-combat-situation)])

(define (describe-location repeated?)
  (define body (Place-get-perceptions (current-location)))

  (info-card body
             (cond
               [(Place? (current-location))
                (format "Otava is in ~a." (Place-shortname (current-location)))]
               [else (format "Otava is ~a." (route-shortname (current-location)))])))

(define (describe-non-combat-situation repeated?)
  (cond
    #;
    (cond
      [(equal? (location-id (current-location)) 'magpie-hill)
       (p
        #:suppress-logging? repeated?
        "Natural rock stairs lead back to Perimeter. There's a small, decrepit industrial-looking building further ahead on the plateau. A small trail leads along the edge of the plateau.")])
    [(null? (current-fragment-id)) (describe-location repeated?)]))

(define (describe-situation repeated?)
  ; (when (location-has-feature? (current-location) 'locked-door)
  ;   (p "The door in the brick wall is locked with a heavy padlock."))
  (cond
    [(current-in-combat?) (describe-combat-situation repeated?)]
    [else (describe-non-combat-situation repeated?)])
  ; (if (flag-set? 'perspective-switched)
  ;   (p #:suppress-logging? repeated? "Otava is the space in which the world appears. Nothing is still, everything fluxating and pulsuating. Atomic particles thrown about by forces of cause and effect. A pulsating heartbeat emanates from deep within the earth's crust.")
  ;   '())
  )
