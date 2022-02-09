#lang at-exp racket

(provide (all-defined-out))

(require
  "../../core/api.rkt"
  "../../locations/locations.rkt")

(define (round-summary mode)
  (define title
    (case mode
      ['begin (string-append "Begin round " (number->string (current-round)))]
      ['continue (string-append "Continue round " (number->string (current-round)))]))

  (define time (world-elapsed-time (current-world)))
  (define time-today (remainder time day-length))
  (define day-number (add1 (quotient time day-length)))

  (define body
    (prune
     (tbody
      (tr (string-append
           "round "
           (number->string (current-round))))
      (tr (string-append
           "day "
           (number->string day-number)
           ", "
           (symbol->string (time-of-day-from-jiffies (world-elapsed-time (current-world))))
           " "
           "("
           (number->string time-today)
           ")"))
      (tr (string-append
           (if (and (not (null? (current-location)))
                    (not (void? (current-location))))
               (get-location-short-description (current-location))
               "N/A")))

      (tr "")
      (tr (string-append
             "hunger: "
             (case (pc-hunger-level)
              ['satiated "satiated"]
              ['not-hungry "not hungry"]
              ['hungry "hungry"]
              ['very-hungry "very hungry"]
              ['starving "starving"]))))))

  (info-card body title))

