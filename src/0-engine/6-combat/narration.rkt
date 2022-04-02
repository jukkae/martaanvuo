#lang at-exp racket

(provide (all-defined-out))

(require
  "combat-event.rkt"
  "stance.rkt"

  "../1-index/state.rkt"

  "../2-core/core.rkt"
  "../2-core/io.rkt"

  "../3-types/actor.rkt"
  "../3-types/pc-actor.rkt"
  "../3-types/status.rkt"
  "../3-types/condition.rkt"
  "../3-types/world.rkt"

  "../4-rules/actors/actor.rkt"
  "../4-rules/pc/pc.rkt"
  )

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
         (when actor
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
                      (else (format "~a ~a" name sign)))))))))


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

(define (display-combat-timeline)
  (define body
    (for/list ([event (current-combat-timeline)])
      (tr (number->string (combat-event-at event)) (combat-event-details event))
      ))
  (info-card body "Combat timeline")
  )
