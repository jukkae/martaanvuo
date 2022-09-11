#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/0-api/types.rkt"

  "../../0-engine/2-core/io.rkt"

  "../../0-engine/4-systems/locations/locations.rkt"
  "../../0-engine/4-systems/pc/pc.rkt"
  "../../0-engine/4-systems/world/time.rkt"

  "../../0-engine/7-state/state.rkt"
  )

(define (display-statusline)
  (define current-day (add1 (exact-floor (/ (world-elapsed-time (current-world)) day-length))))
  (if (not (equal? (location-id (current-location)) 'waiting-room))
      (notice (format
           "~a~a, day ~a, ~a [~a ι].~a"
           (if (current-in-combat?)
               "[In combat] "
               "")
           (get-location-short-description (current-location))
           current-day
           (symbol->string (time-of-day-from-iotas (world-elapsed-time (current-world))))
           (remainder (world-elapsed-time (current-world)) day-length)
           (if (>= (pc-actor-hunger (pc)) hunger-level-hungry)
               (format " Hunger: ~a."
                       (case (pc-hunger-level)
                         ['satiated "satiated"]
                         ['not-hungry "not hungry"]
                         ['hungry "hungry"]
                         ['very-hungry "very hungry"]
                         ['starving "starving"]))
               ""
               )
           ))
        (notice (format
           "Waiting room.")))

  (when (not (empty? (location-items (current-location))))
    (define items (location-items (current-location)))
    (case (length items)
      [(1)
       (define item (car items))
       (define name (item-name item))
       (notice (format "There is ~a here." (string-downcase (format "~a" name))))] ; TODO: all such strings should appear with article prefixed ("*a* blindscraper corpse")
      [else
       (notice "There are multiple items here.")]))

  )
