#lang racket

(provide
 (all-defined-out))

(require racket/lazy-require)

(require rebellion/collection/association-list)

(require "../action.rkt")
(require "../actor.rkt")
(require "../checks.rkt")
(require "../condition.rkt")
(require "../io.rkt")
(require "../item.rkt")
(require "../locations/location.rkt")
(require "../pc.rkt")
(require "../locations/route.rkt")
(require "../state/state.rkt")
(require "../state/logging.rkt")
(require "../stance.rkt")
(require "../status.rkt")
(require "../utils.rkt")
(require "../world.rkt")

(require "../round-resolver/event.rkt"
         "../round-resolver/simulation.rkt"
         "../round-resolver/timeline.rkt")


(lazy-require
 ["state/combat.rkt"
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
 ["round-resolver/event-handler.rkt"
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
                 (string-append (number->string amount) " meal")
                 (string-append (number->string amount) " meals")))

           (info-card
            (list
             (list
              " 1d4 "
              " = "
              (string-append " " amount-string " "))
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
              (list
               (list
                " 1d20 "
                " = "
                (string-append " " (number->string luck-roll) " " )))
              "Luck roll")
             )))
    (if successful?
        'successful
        'failure)))