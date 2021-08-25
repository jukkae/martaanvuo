#lang racket

(provide resolve-action!)

(require racket/lazy-require)

(require rebellion/collection/association-list)

(require "../action.rkt")
(require "../actor.rkt")
(require "../checks.rkt")
(require "../condition.rkt")
(require "../io.rkt")
(require "../item.rkt")
(require "../location.rkt")
(require "../pc.rkt")
(require "../route.rkt")
(require "../state/state.rkt")
(require "../state/logging.rkt")
(require "../stance.rkt")
(require "../status.rkt")
(require "../utils.rkt")
(require "../world.rkt")

(require "../round-resolver/event.rkt"
         "../round-resolver/simulation.rkt"
         "../round-resolver/timeline.rkt")

(require
  "combat-actions.rkt"
  "special-actions.rkt"
  "traverse-action.rkt")


(lazy-require
 ["state/combat.rkt"
  (get-combatant-name
   display-combatant-info
   display-pc-combatant-info
   add-combat-flag
   )])

(lazy-require
 ["locations.rkt"
  (describe-begin-traverse-action
   describe-finish-traverse-action
   describe-cancel-traverse-action
   location-on-enter!
   )])

(lazy-require
 ["round-resolver/event-handler.rkt"
  (handle-interrupting-event!
   )])


; what all should this return?
(define (resolve-action! action)
  (dev-note "elapsed-time is duplicated and broken, TODO has to be fixed")
  (define elapsed-time 0)
  (when (actor-alive? (action-actor action))
    (define result
      (case (action-symbol action)
        ; "special" actions first
        ['end-run (resolve-special-action! action)]
        ['back-off (resolve-special-action! action)]
        ['win-game (resolve-special-action! action)]
        ['skip (resolve-special-action! action)]
        
        ['go-to-location (resolve-go-to-action! action)]
        ['traverse (resolve-traverse-action! action)]
        ['cancel-traverse (resolve-cancel-traverse-action! action)]
      
      
        ; the rest
        ['melee (resolve-melee-action! action)]
        ['shoot (resolve-shoot-action! action)]
        ['forage (resolve-forage-action! action)]
        ['sleep
         (p "Otava makes camp.")
         'ok]
      
        ['flee (resolve-flee-action! action)]
        ['break-free (resolve-break-free-action! action)]

        ['anklebreaker (resolve-anklebreaker-action! action)]
        ['pull-under (resolve-pull-under-action! action)]
        ['release-grip 'grip-released]

        ['go-to-engaged (resolve-go-to-engaged-action! action)]
        ['go-to-close (resolve-go-to-close-action! action)]


        ['inflict-status
         (define target (action-target action))
         (define status (car (action-details action)))
         (when (status? status)
           (when (eq? (status-type status) 'bound)
             (p "The Grabberkin seems to realize its grip is loosening. Its rotting fingers curl around Otava's ankle again with dreadful might.")))
           
         (inflict-status! target status)
         'ok]

        ['modify-status
         (define target (action-target action))
         (define status (car (action-details action)))
         (when (eq? (status-type status) 'bound) ; this is shit, refactor
           (p "The Grabberkin seems to realize its grip is loosening. Its rotting fingers curl around Otava's ankle again with dreadful might.")
           (define amount (status-lifetime status))
           (modify-actor-status-lifetime target 'bound amount)
           )
         'ok]

        ['inflict-condition
         (define target (action-target action))
         (define condition (car (action-details action)))
         (displayln "action-resolver: resolve-action!: inflict-condition: TODO")
         #;(when (eq? (status-type status) 'bound)
             (p "The Grabberkin seems to realize its grip is loosening. Its rotting fingers curl around Otava's ankle again with dreadful might."))
         #;(inflict-status! target status)
         'ok]

        [else
         (error (string-append "resolve-action!: unknown action type " (symbol->string (action-symbol action))))]))


    ; what a hack
    (when (timeline? result)
      (handle-pc-action-interrupted! result)
      (set! result 'interrupted))

    (when (eq? 'interrupted result) (set-pending-action! action elapsed-time))
    result
    ))

(define (set-pending-action! action elapsed-time)
  (define time-left (- (action-duration action) elapsed-time))
  (define pending-action action)
  (set-action-duration! pending-action time-left)
  (set-action-details! pending-action (append-element (action-details pending-action) 'pending))
  (current-pending-action pending-action))

(define (resolve-go-to-action! action)
  (define elapsed-time 0)
  (cond ((not (pending? action))
         (describe-begin-traverse-action action)))

  (define result
    (let/ec return
      (begin
        ; begin advancing time
        (define timeline
          (advance-time-until-next-interesting-event! (action-duration action)))
        (set! elapsed-time (timeline-duration timeline))

        #;(narrate-timeline timeline)

        (when (eq? (timeline-metadata timeline) 'interrupted)
          (handle-pc-action-interrupted! timeline)
          (return 'interrupted))


        (define next-location (action-target action))
        (move-pc-to-location! next-location)
        (location-on-enter! (current-location))

        (describe-finish-traverse-action action)
                          
        (when (not (null? (location-items (action-target action))))
          (pick-up-items!))

        'ok)
      ))
  result)






(define (handle-pc-action-interrupted! timeline)
  (define interrupting-events
    (filter
     (λ (event) (event-interrupting? event))
     (timeline-events timeline)))
  
  (cond ((eq? (length interrupting-events) 1)
         (define event (car interrupting-events))
         (handle-interrupting-event! event)
         )
        (else
         (dev-note "handle-pc-action-interrupted!: unexpected amount of interrupting events.")))
  )




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

(define (resolve-break-free-action! action)
  (define actor (action-actor action))
  (define details (action-details action))

  (define str-mod (vector-ref (association-list-ref details 'str-mod) 0))

  (define target (action-target action))
  (define target-stance (actor-stance target))

  
  (define statuses (actor-statuses actor))
  (define actor-bound-status
    (findf (λ (status) (eq? (status-type status) 'bound))
           statuses))

  (define target-number (status-lifetime actor-bound-status))

  (define dice-sides 10)
  (define bonus str-mod)
  (define roll (d 1 dice-sides))
  (define result (+ roll bonus))

  (define success?
    (cond ((= roll 1) #f)
          ((= roll dice-sides) #t)
          (else (> result target-number))))

  (define success-string
    (if success?
        "success"
        "failure"))

  (displayln
   (string-append "["
                  "Resolution: "
                  "1d10 + bonus > TN: "
                  (number->string roll)
                  " + "
                  (number->string bonus)
                  " = "
                  (number->string result)
                  " > "
                  (number->string target-number)
                  " - "
                  success-string
                  "]"))
  ; crit = nat MAX = always succeed,
  ; crit fail = nat 1 = always fail, avoid hard failures?
  (wait-for-confirm)
  (if success?
      (begin
        (displayln "Otava pulls her ankle free and stumbles back, just far enough to be out of reach of the writhing, searching hands.")
        (award-xp! 4)
        'end-combat)
      (begin
        (displayln "The grip is still too strong for Otava to break it.")
        (award-xp! 1)
        'failed)))


; skinnable, but in a sense generic action
(define (resolve-flee-action! action)
  (cond ((pc-actor? (action-actor action))
         (p "Otava turns her back to run.")
         (define skill (get-trait (pc) "athletics-skill"))

         (define stance-range-values '())
         (for ([enemy (get-current-enemies)])
           (define stance (actor-stance enemy))
           (define value (get-stance-range-numeric-value (stance-range stance)))
           (set! stance-range-values (append-element stance-range-values value)))
         (define target-number
           ; if there's an enemy in engaged range, then more difficult check
           (if (member 0 stance-range-values)
               10
               8))
           
         (define success? (skill-check "Athletics" skill target-number))
         (if success?
             (begin ; TODO wouldn't it be cool if only failure was explicitly noted :D
               (p "She dives behind a small bush and waits.")
               (wait-for-confirm)
               (if (luck-check)
                   (p "PASS")
                   (p "FAIL"))
               (p "Nothing seems to be following her.")
               (award-xp! 3 "for a working survival instinct")
               'end-combat)
             (begin
               (p "Otava's foot gets caught on a root. She falls face down in the mud.")
               (actor-add-status! (pc) (status 'fallen 1))
               (display-pc-combatant-info (pc))
               (wait-for-confirm)
               'failure))
         )

        (else ; not a pc actor
         (p
          (string-append
           (get-combatant-name (action-actor action))
           " tries to run."))
         (define skill 1)
         (define stance (actor-stance (action-actor action)))
         (define value (get-stance-range-numeric-value (stance-range stance)))
         (define target-number
           (if (= value 0)
               10
               8))

         (define success? (skill-check "Athletics" skill target-number))
         (if success?
             ; TODO this fails if there are multiple enemies!
             (begin
               (p "The Blindscraper skitters away and disappears in the foliage.")
               (award-xp! 1)
               'escape-from-combat)
             (begin
               (p "It is fast, but not fast enough.")
               (actor-add-status! (action-actor action) (status 'fallen 1))
               (display-combatant-info (action-actor action))
               'failure))
         )))
