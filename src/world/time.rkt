#lang at-exp racket

(provide (all-defined-out))

(define (time-of-day-from-jiffies jiffies)
  (define jiffies-of-current-day (remainder jiffies day-length))
  (define time-of-day
    (cond ((< jiffies-of-current-day 100) 'morning)
          ((< jiffies-of-current-day 200) 'midday)
          ((< jiffies-of-current-day 300) 'afternoon)
          ((< jiffies-of-current-day 400) 'evening)
          ((< jiffies-of-current-day 600) 'night)))
  time-of-day)

(define day-length 600)
