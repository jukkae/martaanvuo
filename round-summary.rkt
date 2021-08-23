#lang racket

(provide (all-defined-out))

(require "api.rkt")

(define (round-summary situation)
  (define body (list
                (list " round "
                      (string-append
                       " "
                       (number->string (situation-round situation))
                       " "))
                (list " current location "
                      (string-append
                       " "
                       #;(get-location-name-from-location (current-location))
                       (if (and (not (null? (current-location)))
                                (not (void? (current-location))))
                           (get-location-short-description (current-location))
                           "N/A")
                       " "))
                (list " time of day " (string-append " " (symbol->string (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*)))) " "))
                (list " elapsed time (total) " (string-append " " (number->string (world-elapsed-time (situation-world *situation*))) " "))
                ))
  (info-card body (string-append "Begin round " (number->string (situation-round situation))))
  (display-location-info-card (current-location) "Current location"))

