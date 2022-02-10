#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require
  "stance.rkt"

  "../actors/actor.rkt"
  "../actors/condition.rkt"
  "../actors/pc-actor.rkt"
  "../actors/status.rkt"

  "../core/session.rkt"
  "../core/io.rkt"
  "../core/utils.rkt"

  "../pc/pc.rkt"

  "../world/world.rkt")

(lazy-require
 ["../state/state.rkt"  (current-in-combat?
                         current-log
                         current-location
                         current-combat-timeline
                         current-world
                         get-current-enemies
                         pc)])

(lazy-require ["../state/logging.rkt"
               (next-chapter!)])

(define (display-pc-combatant-info actor)
  (define name (get-combatant-name actor))
  (define body
    (tbody
     (tr
      "HP"
      (format "~a/~a"
              (actor-hp actor)
              (actor-max-hp actor)))))

  (when (not (null? (actor-statuses actor)))
    (define statuses (actor-statuses actor))
    (define statuses-strings
      (for/list ([status statuses])
        (format "[~a (~a)]"
                (status-type status)
                (status-lifetime status))))

    (define statuses-list
      (tr "statuses"
          (string-join statuses-strings)))
    (set! body (append-element body statuses-list)))

  (when (not (null? (actor-conditions actor)))
    (define conditions (actor-conditions actor))
    (define conditions-strings
      (for/list ([condition conditions])
        (format "[~a]" (condition-type condition))))

    (define conditions-list
      (tr "conditions"
          (string-join conditions-strings)))
    (set! body (append-element body conditions-list)))
  (info-card
   body
   name))

(define (display-combatant-info actor)
  (if (pc-actor? actor)
      (display-pc-combatant-info actor)
      (when (actor-alive? actor)
        (display-non-pc-combatant-info actor))))

(define (describe-combat-situation)
  ; show cards
  (notice "Otava is in combat.")
  (for ([enemy (get-current-enemies)])
    (display-combatant-info enemy))
  (display-pc-combatant-info (pc))

  ; narrate
  (define enemy-names
    (for/list ([enemy (get-current-enemies)])
      (actor-name enemy)))

  (when (= (length (get-current-enemies)) 1)
    (define enemy (car (get-current-enemies)))
    (case (stance-range (actor-stance enemy))
      ['mid
       (p "The " (car enemy-names) " is a couple of paces away from Otava, trying to get closer. Otava is holding her revolver.")]
      #;['close ; this is specific to enemy type etc
       (p "The " (car enemy-names) " is closing in fast, its claws flickering as it jumps from a rock onto the trunk of a nearby tree.")])))

(define (get-combatant-name actor)
  (cond ((pc-actor? actor)
         "Otava")
        (else
         (define stance (actor-stance actor))
         (cond ((= (length (get-current-enemies)) 1)
                (actor-name actor))
               (else
                (define name (actor-name actor))
                (define sign
                  (if stance
                      (stance-sign stance)
                      ""))
                (cond ((eq? "" sign)
                       name)
                      (else (format "~a ~a" name sign))))))))


(define (display-non-pc-combatant-info actor)
  (define stance (actor-stance actor))
  (define name (get-combatant-name actor))
  (define hide-hp?
    (if (hash-ref (actor-traits actor) "hp-hidden" #f)
        #t
        #f))

  (define body
    (case (actor-name actor)
      [("Grabberkin")
       (tbody
        (tr
         "HP"
         (if hide-hp?
             "???"
             (format "~a/~a" (actor-hp actor) (actor-max-hp actor))))
        (if (not (null? stance))
         (tr
          "range"
          (format "~a" (stance-range stance)))
         (tr
          "range"
          "N/A")))]

      [("Blindscraper")
       (tbody
        (tr
         "size"
         (get-trait actor "size"))
        (if (not (null? stance))
         (tr
          "range"
          (symbol->string (stance-range stance)))
         (list
          "range"
          "N/A")))]))

  (when (not (null? (actor-statuses actor)))
    (define statuses (actor-statuses actor))
    (define statuses-list
      (tr "statuses" (~s statuses)))
    (set! body (append-element body statuses-list)))
  (info-card
   body
   name))


(serializable-struct
 combat-event
 (details
  at)
 #:constructor-name combat-event*
 #:transparent)

(define (make-combat-event
         details)
  (combat-event* details (world-elapsed-time (current-world))))


(define (add-combat-event text)
  (current-combat-timeline (append-element (current-combat-timeline) (make-combat-event text))))

(define (begin-combat!)
  (wait-for-confirm)
  (next-chapter!)
  (current-in-combat? #t)

  (current-session-times-in-combat++)
  (when (= (current-session-times-in-combat) 3)
    (notice "Attainment: Way of Blood"))
  (when (= (current-session-times-in-combat) 7)
    (notice "Attainment: Way of Carnage"))
  (when (= (current-session-times-in-combat) 16)
    (notice "Attainment: Way of Bloodshed"))
  (when (= (current-session-times-in-combat) 31)
    (notice "Attainment: Way of Death"))
  (when (= (current-session-times-in-combat) 100)
    (notice "Attainment: Hecatomb")) ; award this for 100 *kills*
  ; (when (not (session-flag-set? 'got-in-combat))
  ;   (set-session-flag 'got-in-combat)
  (current-session-score-dice++)
  ;   (notice "Attainment: Fighter"))
  (add-combat-event "combat started"))

(define (display-combat-timeline)
  (define body
    (for/list ([event (current-combat-timeline)])
      (tr (number->string (combat-event-at event)) (combat-event-details event))
    ))
  (info-card body "Combat timeline")
  )

(define (end-combat!)
  (define success-text (if (pc-is-alive?) "Otava survived." "Otava died."))
  (notice (format "Combat finished. [~a]" success-text))
  (add-combat-event "combat finished")
  (display-combat-timeline)
  (current-in-combat? #f)
  (current-combat-timeline '())
  (wait-for-confirm)
  (when (pc-is-alive?)
    (next-chapter!)))
