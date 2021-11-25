#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require
  "../actors/actor.rkt"
  "../actors/condition.rkt"
  "../actors/pc-actor.rkt"
  "../actors/stance.rkt"
  "../actors/status.rkt"

  "../core/session.rkt"
  "../core/io.rkt"
  "../core/utils.rkt"

  "../world/world.rkt")

(lazy-require
 ["state.rkt" (current-in-combat?
               current-log
               current-location
               current-combat-timeline
               current-world
               get-current-enemies
               pc)])

(lazy-require ["logging.rkt"
               (next-chapter!)])

(define (display-pc-combatant-info actor)
  (define name (get-combatant-name actor))
  (define body
    (list
     (list
      " HP "
      (string-append " "
                     (number->string (actor-hp actor))
                     "/"
                     (number->string (actor-max-hp actor))
                     " "
                     ))))

  (when (not (null? (actor-statuses actor)))
    (define statuses (actor-statuses actor))
    (define statuses-strings
      (for/list ([status statuses])
        (string-append "["
                       (symbol->string (status-type status))
                       " ("
                       (number->string (status-lifetime status))
                       ")]")))
    
    (define statuses-list
      (list " statuses "
            (string-append " " (string-join statuses-strings) " ")))
    (set! body (append-element body statuses-list)))

  (when (not (null? (actor-conditions actor)))
    (define conditions (actor-conditions actor))
    (define conditions-strings
      (for/list ([condition conditions])
        (string-append "["
                       (symbol->string (condition-type condition))
                       "]")))
    
    (define conditions-list
      (list " conditions "
            (string-append " " (string-join conditions-strings) " ")))
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
       (p "The " (car enemy-names) " is closing in fast, its claws flickering as it jumps from a rock onto the trunk of a nearby tree.")]
      )
    )
  )

(define (get-combatant-name actor)
  (cond ((pc-actor? actor)
         "Otava")
        (else
         (define stance (actor-stance actor))
         (cond ((= (length (get-current-enemies)) 1)
                (string-append (actor-name actor)))
               (else
                (define name (actor-name actor))
                (define sign
                  (if stance
                      (stance-sign stance)
                      ""))
                (cond ((eq? "" sign)
                       name)
                      (else (string-append name " " sign))))))))


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
       (list
        (list
         " HP "
         (if hide-hp?
             " ??? "
             (string-append " "
                            (number->string (actor-hp actor))
                            "/"
                            (number->string (actor-max-hp actor))
                            " "
                            ))))]
      [("Blindscraper")
       (list
        (list
         " size "
         (string-append " "
                        (get-trait actor "size")
                        " "
                        ))
        #;(list
           " location "
           (string-append " " (stance-location stance) " "))
        (if (not (null? stance))
            (list
             " range "
             (string-append " " (symbol->string (stance-range stance)) " "))
            (list
             " range "
             (string-append " " "N/A" " ")))
        

        )]))

  (when (not (null? (actor-statuses actor)))
    (define statuses (actor-statuses actor))
    (define statuses-list
      (list " statuses " (~s statuses)))
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
  (add-combat-event "combat started!"))

(define (end-combat!)
  (notice "Combat finished.")
  (dev-note "timeline:")
  (displayln (current-combat-timeline))
  (current-in-combat? #f)
  (current-combat-timeline '())
  (wait-for-confirm)
  (next-chapter!))