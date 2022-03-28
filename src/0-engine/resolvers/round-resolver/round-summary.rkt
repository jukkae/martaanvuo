#lang at-exp racket

(provide (all-defined-out))

(require
  "../../core/io.rkt"
  "../../core/utils.rkt"
  "../../pc/pc.rkt"
  "../../locations/locations.rkt"
  "../../world/0-types/world.rkt"
  "../../world/time.rkt")

(require
  racket/lazy-require)

(lazy-require
  ["../../state/state.rkt"
    (current-round
     current-world
     current-location)])

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

