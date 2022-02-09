#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "quest.rkt"
         "../core/io.rkt")

(lazy-require
 ["../state/state.rkt"
  (add-quest!
   quest-exists?)])

(define (create-quest quest-symbol)
  (define q
    (case quest-symbol
      ['pay-off-debt
       (quest 'pay-off-debt
              "Debt to Mediator"
              "in progress"
              "unsettled: 10,111 grams of gold" ; MAKE A FUCKING PLOT POINT OF THIS
              10111)]
      ['anthead-monograph
       (quest 'anthead-monograph
              "Anthead Monograph"
              "in progress"
              "find the Maw, find the Book"
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

    (define body
      (format-quest-for-card q))

    (info-card
     (list body)
     "New quest" #f)

    (wait-for-confirm)))
