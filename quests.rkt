#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "io.rkt")
(require "quest.rkt")

(lazy-require
 ["situation.rkt"
  (add-quest!
   quest-exists?)])

(define (create-quest quest-symbol)
  (define q
    (case quest-symbol
      ['pay-off-debt
       (quest 'pay-off-debt
              "Debt to Collector"
              "in progress"
              "unsettled: 10,111 g of Martaanvuo gold"
              10111)] ; gold-198 has a short halflife, around 2.7 days, -> temporal anomaly
      ['anthead-monograph
       (quest 'anthead-monograph
              "Anthead Monograph"
              "???"
              "a book? who/WHAT is Stiltman?? fee for what?"
              '())]
      ['grabberkin-finger
       (quest 'grabberkin-finger
              "Grabberkin finger"
              "in progress"
              "Anthill: 29 g gold / each"
              '())]))

  (when (not (quest-exists? quest-symbol))
    (add-quest! q)
    (case quest-symbol
      ['pay-off-debt
       (p "She's getting closer to the Martaanvuo Anomaly, too close to be comfortable. But the Debt is still there, so she doesn't have much choice.")])
  

    (define body
      (format-quest-for-card q))

    (info-card
     (list body)
     "New quest")

    (wait-for-confirm)))