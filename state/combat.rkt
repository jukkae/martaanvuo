#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "../actor.rkt"
         "../condition.rkt"
         "../io.rkt"
         "../location.rkt"
         "../pc.rkt"
         "../stance.rkt"
         "../status.rkt"
         "../utils.rkt")

(lazy-require
 ["state.rkt" (current-in-combat?
               current-log
               current-location
               get-current-enemies
               pc)])

(define (clean-up-dead-actor! actor)
  (remove-actor-from-location! (current-location) actor)
  (define corpse (cons 'corpse "Corpse (TODO)"))
  (add-feature-to-location! (current-location) corpse))


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
  (notice "Otava is in combat.")
  (for ([enemy (get-current-enemies)])
    (display-combatant-info enemy)
    
    )
  (display-pc-combatant-info (pc))
  )

(define (get-combatant-name actor)
  (cond ((pc-actor? actor)
         "Otava")
        (else
         (define stance (actor-stance actor))
         (cond ((= (length (get-current-enemies)) 1)
                (append-string (actor-name actor)))
               (else
                (define name (actor-name actor))
                (define sign
                  (if stance
                      (stance-sign stance)
                      ""))
                (append-string name " " sign))))))


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


(define *combat-flags* '())
(define (add-combat-flag flag)
  (set! *combat-flags* (append-element *combat-flags* flag)))

(define (begin-combat!)
  (dev-note "combat flags are broken!")
  (current-in-combat? #t)
  (set! *combat-flags* '()))

(define (end-combat!)
  (notice "Combat finished.")
  (dev-note "combat flags are broken!")
  (current-in-combat? #f)
  (set! *combat-flags* '()))