#lang at-exp typed/racket

(provide (all-defined-out))

(define-type TimeOfDay (U 'morning 'midday 'afternoon 'evening 'night))

(: time-of-day-from-iotas (-> Integer TimeOfDay))
(define (time-of-day-from-iotas iotas)
  (define iotas-of-current-day (remainder iotas day-length))
  (define time-of-day
    (cond ((< iotas-of-current-day 100) 'morning)
          ((< iotas-of-current-day 200) 'midday)
          ((< iotas-of-current-day 300) 'afternoon)
          ((< iotas-of-current-day 400) 'evening)
          ((< iotas-of-current-day 600) 'night)
          [else 'night])) ; TODO: this shouldn't happen – refine types!
  time-of-day)

(define day-length 600)
