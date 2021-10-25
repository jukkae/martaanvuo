#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "io.rkt")
(require "quest.rkt")

(lazy-require
 ["state/state.rkt"
  (add-quest!
   quest-exists?)])

(define (create-quest quest-symbol)
  (define q
    (case quest-symbol
      ['pay-off-debt
       (quest 'pay-off-debt
              "Debt to Collector"
              "in progress"
              "unsettled: 10,111 g of gold"
              10111)]
      ['anthead-monograph
       (quest 'anthead-monograph
              "Anthead Monograph"
              "???"
              "a book? who/WHAT is Stiltman?? fee for what?"
              '())]
      ['loot-the-cache
       (quest 'loot-the-cache
              "Loot the Cache"
              "in progress"
              "power plant (?) on the hill"
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
       (p "She's getting closer to Martaanvuo anomaly. If what she's pieced together is correct, she'll be able to pay off the rest of her debt to Collector, and still have some left over.")])
  

    (define body
      (format-quest-for-card q))

    (info-card
     (list body)
     "New quest")

    (wait-for-confirm)))