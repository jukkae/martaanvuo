#lang racket

(provide
 (all-defined-out))

(require racket/lazy-require)

(require
  "../actors/actor.rkt"
  "../core/checks.rkt"
  "../core/io.rkt"
  "../state/state.rkt"
  "../core/utils.rkt")


(lazy-require
 ["../state/combat.rkt"
  (get-combatant-name
   display-combatant-info
   display-pc-combatant-info
   add-combat-flag
   )])

(lazy-require
 ["../locations/locations.rkt"
  (describe-begin-traverse-action
   describe-finish-traverse-action
   describe-cancel-traverse-action
   location-on-enter!
   )])

(lazy-require
 ["../round-resolver/event-handler.rkt"
  (handle-interrupting-event!
   )])

(define (resolve-sleep-action! action)
  (p "Otava makes camp.")
  'ok)

; just a skill check in a fancy coat
(define (resolve-forage-action! action)

  (begin
    (define skill 0)
    (define target 8)

    (define successful? (skill-check "Forage" skill target))
    (cond (successful?
           (define amount (d 1 4)) ; portions = days of survival
           (define amount-string
             (if (= amount 1)
                 (format "~a meal" amount)
                 (format "~a meals" amount)))

           (info-card
            (tbody
             (tr
              " 1d4 "
              " = "
              (format " ~a " amount-string))
             )
            "Forage results roll")
           (p "After some time, Otava finds some edible fruits and roots. (" (number->string amount) " meals.)")
           (define item (list 'food (list amount)))
           (add-item-to-inventory! (pc) item)
           )
          (else
           (begin
             (p "Despite spending a while, Otava can't find anything to eat.")
             (define luck-roll (d 1 20))
             (info-card
              (tbody
               (tr
                " 1d20 "
                " = "
                (format " ~a " luck-roll)))
              "Luck roll")
             )))
    (if successful?
        'successful
        'failure)))
