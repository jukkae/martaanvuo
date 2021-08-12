#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "io.rkt")
(require "quest.rkt")

(lazy-require
 ["situation.rkt"
  (add-quest!)])

(define (create-quest quest-symbol)
  (define q
    (case quest-symbol
      ['pay-off-debt
       (quest 'pay-off-debt
              "pay off Debt to Collector"
              "in progress"
              "unsettled: 10,111 g of Martaanvuo gold"
              10111)] ; gold-198 has a short halflife, around 2.7 days, -> temporal anomaly
      ['the-anthead
       (quest 'the-anthead
              "seek the Anthead Girl"
              "not started"
              "\"not ready yet\", whatever."
              '())]
      ['pulverization-monograph
       (quest 'pulverization-monograph
              "Pulverization Monograph"
              "???"
              "a book? who/WHAT is Stiltman?? fee agreed for what?"
              '())]))
  (add-quest! q)


  (case quest-symbol
    ['pay-off-debt
     (paragraph "She's getting closer to the Martaanvuo Anomaly, too close to be comfortable. But the Debt is still there, so she doesn't have much choice.")])
  

  (define body
    (format-quest-for-card q))

  (info-card
   (list body)
   "New quest")

  (wait-for-confirm))