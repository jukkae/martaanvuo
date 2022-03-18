#lang at-exp racket

(provide (all-defined-out))

(define (time-of-day-from-iotas iotas)
  (define iotas-of-current-day (remainder iotas day-length))
  (define time-of-day
    (cond ((< iotas-of-current-day 100) 'morning)
          ((< iotas-of-current-day 200) 'midday)
          ((< iotas-of-current-day 300) 'afternoon)
          ((< iotas-of-current-day 400) 'evening)
          ((< iotas-of-current-day 600) 'night)))
  time-of-day)

(define day-length 600)
