#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/0-api/api.rkt"
  )

(define (display-pc-combatant-info actor)
  (define name (get-combatant-name actor))
  (define body
    (tbody
     (tr
      "Condition [perceived with basic homeostasis]"
      (if (> (actor-hp (pc)) 1) "Fine" "Not fine")
      )
     #;(tr
      "HP        [perceived with nociception]"
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
  #;(define enemy-names
    (for/list ([enemy (get-current-enemies)])
      (actor-name enemy)))

  #;(when (= (length (get-current-enemies)) 1)
    (define enemy (car (get-current-enemies)))
    (case (stance-range (actor-stance enemy))
      #;['mid
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
       (define unpruned-rows '())
       (when (pc-has-sense-organ? 'eyes)
         (when (not (null? stance))
           (set! unpruned-rows
                 (append-element unpruned-rows
                                 (tr
                                  "range [perceived with eyes]"
                                  (symbol->string (stance-range stance)))))
           )
         (set! unpruned-rows
                 (append-element unpruned-rows
                                 (tr
                                  "size  [perceived with eyes]"
                                  (format "~a" (actor-size actor)))))
         (set! unpruned-rows
               (append-element unpruned-rows
                               (tr
                                "HP    [perceived with eyes]"
                                (if hide-hp?
                                    "???"
                                    (format "~a/~a" (actor-hp actor) (actor-max-hp actor))))))
         )
       (when (pc-has-sense-organ? 'echolocation)
         (set! unpruned-rows
               (append-element unpruned-rows
                               (tr
                                "range [perceived with echolocation]"
                                (symbol->string (stance-range stance)))))
         )

        (when (and (pc-has-sense-organ? 'nose) (< (actor-hp actor) (actor-max-hp actor)))
         (set! unpruned-rows
               (append-element unpruned-rows
                               (tr
                                "HP    [perceived with nose]"
                                (format "~a/~a" (actor-hp actor) (actor-max-hp actor)))))
         )

       unpruned-rows]

      [("Human fighter")
       (tbody
        (tr
         "size"
         (actor-size actor))
        (if (not (null? stance))
            (tr
             "range"
             (symbol->string (stance-range stance)))
            (list
             "range"
             "N/A")))]

      [("Voidfloater")
       (define unpruned-rows '())
       (when (pc-has-sense-organ? 'eyes)
         (when (not (null? stance))
           (set! unpruned-rows
                 (append-element unpruned-rows
                                 (tr
                                  "range [perceived with eyes]"
                                  (symbol->string (stance-range stance)))))
           (set! unpruned-rows
                 (append-element unpruned-rows
                                 (tr
                                  "size  [perceived with eyes]"
                                  (format "~a" (actor-size actor)))))
           )
         (case (get-current-light-level)
          ['bright
           (set! unpruned-rows
               (append-element unpruned-rows
                               (tr
                                "HP    [perceived with eyes]"
                                (format "~a/~a" (actor-hp actor) (actor-max-hp actor)))))]
          [else
           (set! unpruned-rows
               (append-element unpruned-rows
                               (tr
                                "HP    [perceived with eyes]"
                                (format "too dark to see"))))])
         )
       (when (pc-has-sense-organ? 'echolocation)
         (set! unpruned-rows
               (append-element unpruned-rows
                               (tr
                                "range [perceived with echolocation]"
                                (symbol->string (stance-range stance)))))
         )

        (when (and (pc-has-sense-organ? 'nose) (< (actor-hp actor) (actor-max-hp actor)))
         (set! unpruned-rows
               (append-element unpruned-rows
                               (tr
                                "HP    [perceived with nose]"
                                (format "~a/~a" (actor-hp actor) (actor-max-hp actor)))))
         )

       unpruned-rows
       ]

      [else
        (dev-note (format "Unknown actor: ~a" (actor-name actor)))
        (tbody
        (tr
         "size"
         (actor-size actor))
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
