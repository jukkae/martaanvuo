#lang at-exp racket

(require
  "../../../1-index/content.rkt"

  "../../../3-types/action.rkt"
  "../../../3-types/choice.rkt"
  "../../../3-types/location.rkt"
  "../../../3-types/route.rkt"

  "../../../7-state/state.rkt"
  )

(provide make-forage-choice)
(define (make-forage-choice)
  (make-choice
   'forage
   "Forage. [once per day]"
   (λ () (make-action
          #:symbol 'forage
          #:actor (pc)
          #:duration 100
          #:tags '(downtime)
          #:resolution-rules
          `(
            (define skill 0)
            (define target-number 8)

            (define successful? (skill-check "Forage" skill target-number))
            (mark-once-per-day-action-done! 'forage)
            (cond (successful?
                   (define amount (d 1 2))
                   (define amount-string
                     (if (= amount 1)
                         (format "~a handful" amount)
                         (format "~a handfuls" amount)))

                   (info-card
                    (tbody
                     (tr
                      "1d4"
                      "="
                      (format "~a" amount-string))
                     )
                    "Forage results roll")
                   (p "Otava finds crowberries and bogberries. (" (number->string amount) " handfuls.)")
                   (define item (make-item 'fresh-berries #:amount amount))
                   (add-item! item)
                   )
                  )
            (if successful?
                'successful
                'failure)
            )

          ))))