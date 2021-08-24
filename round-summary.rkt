#lang racket

(provide (all-defined-out))

(require "api.rkt")

(define (round-summary mode)
  (define title
    (case mode
      ['begin (string-append "Begin round " (number->string (current-round)))]
      ['continue (string-append "Continue round " (number->string (current-round)))]))
  
  (define body
    (prune
     (list
      (list " round "
            (string-append
             " "
             (number->string (current-round))
             " "))
      (list " current location "
            (string-append
             " "
             (if (and (not (null? (current-location)))
                      (not (void? (current-location))))
                 (get-location-short-description (current-location))
                 "N/A")
             " "))
      (list
       " time of day "
       (string-append " " (symbol->string (time-of-day-from-jiffies (world-elapsed-time (current-world)))) " "))
      (list
       " elapsed time (total) "
       (string-append " " (number->string (world-elapsed-time (current-world))) " ")))))
  (info-card body title))

