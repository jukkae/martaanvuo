#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/2-core/io.rkt"
  "../../0-engine/2-core/core.rkt"

  "../../0-engine/3-types/world.rkt"

  "../../0-engine/4-systems/pc/pc.rkt"
  "../../0-engine/4-systems/locations/locations.rkt"
  "../../0-engine/4-systems/world/time.rkt"

  "../../0-engine/7-state/state/state.rkt"
  )


(define (round-summary mode)
  (define title
    (case mode
      ['begin (format "Begin round ~a" (current-round))]
      ['continue (format "Continue round ~a" (current-round))]))

  (define time (world-elapsed-time (current-world)))
  (define time-today (remainder time day-length))
  (define day-number (add1 (quotient time day-length)))

  (define body
    (prune
     (tbody
      (tr (format "round ~a" (current-round)))
      (tr (format "day ~a, ~a (~a)"
           day-number
           (time-of-day-from-iotas (world-elapsed-time (current-world)))
           time-today))
      (tr (if (and (not (null? (current-location)))
                    (not (void? (current-location))))
              (get-location-short-description (current-location))
              "N/A"))

      (tr "")
      (tr (format
             "hunger: ~a"
             (case (pc-hunger-level)
              ['satiated "satiated"]
              ['not-hungry "not hungry"]
              ['hungry "hungry"]
              ['very-hungry "very hungry"]
              ['starving "starving"]))))))

  (info-card body title))

