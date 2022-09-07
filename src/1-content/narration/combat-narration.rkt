#lang at-exp rackjure

(provide (all-defined-out))

(require
  "../../0-engine/0-api/api.rkt"
  )



(define (display-pc-combatant-info actor)
  (define name (get-combatant-name actor))
  (define body '())

  (when [pc-has-sense-organ? 'basic-homeostasis]
    (define percepts '())
    (when (pc-envenomed-peaking?)
      (append-element! percepts "Weak and dazed [envenomed – acute]"))
    (when (<= (actor-hp (pc)) 1)
      (append-element! percepts "Really fucking bad [severe blood loss – low HP]"))
    ; (when (actor-has-condition-of-type? (pc) 'envenomed)
    ;   (append-element! percepts "Bad"))
    (when (empty? percepts)
      (append-element! percepts "Fine"))
    (for ([percept percepts])
      (append-element! body
        (tr "basic homeostasis" "base feeling" percept)))
    )

  (when (and (pc-has-sense-organ? 'eyes)
             (not (eq? (get-current-light-level) 'pitch-black)))
    (define percept
      (cond [(= (actor-hp actor) (actor-max-hp actor))
             "Perfect"]
            [(> (actor-hp actor) (/ (actor-max-hp actor) 2))
             "Somewhat damaged"]
            [else "Bad"]))
    (append-element! body
      (tr "eyes" "organism condition" percept)))

  (when [pc-has-sense-organ? 'nociception]
    (append-element! body
                     (tr
                      "nociception"
                      "HP"
                      (format "~a/~a"
                              (actor-hp actor)
                              (actor-max-hp actor))))
    )

  (when (not (null? (actor-statuses actor)))
    (define statuses (actor-statuses actor))
    (define statuses-strings
      (for/list ([status statuses])
        (format "[~a (~a)]"
                (status-type status)
                (status-lifetime status))))

    (define statuses-list
      (tr "" "statuses"
          (string-join statuses-strings)))
    (set! body (append-element body statuses-list)))

  (when (not (null? (actor-conditions actor)))
    (define conditions (actor-conditions actor))
    (define conditions-strings
      (for/list ([condition conditions])
        (format "[~a~a (age: ~a ι)]"
          (condition-type condition)
          (if (Illness? condition)
            (format " – ~a" (Illness-state condition))
            "")
          (condition-age condition)
        )))

    (define conditions-list
      (tr "" "conditions"
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
  (newline)
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
      #;['nearby
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
  (define body '())
  (for ([sense-organ (pc-actor-sense-organs (pc))])
    (case (SenseOrgan-id sense-organ)
      ['eyes
       (define eyes-text (format "eyes [~a]" (get-current-light-level)))
       (append-element! body (tr
                              eyes-text
                              "range"
                              (symbol->string (stance-range stance))))
       (append-element! body (tr
                              eyes-text
                              "size"
                              (format "~a" (actor-size actor))))
       (case (get-current-light-level)
         ['bright
          (append-element! body
                           (tr
                            eyes-text
                            "HP"
                            (format "~a/~a" (actor-hp actor) (actor-max-hp actor))))]
         ['dark
          (append-element! body
                           (cond [(or (eq? (stance-range stance) 'engaged)
                                      (eq? (stance-range stance) 'adjacent)
                                      (eq? (stance-range stance) 'close)
                                      (eq? (stance-range stance) 'nearby))
                                  (tr
                                   eyes-text
                                   "HP"
                                   (format "~a/~a" (actor-hp actor) (actor-max-hp actor)))
                                  ]
                                 [else
                                  (tr
                                   eyes-text
                                   "HP"
                                   (format "too dark to see this far"))])
                           )]
         ['pitch-black
          (append-element! body
                           (tr
                            eyes-text
                            "HP"
                            (format "too dark to see")))])
       ]
      ['sonar
       (append-element! body
                        (cond [(or (eq? (stance-range stance) 'engaged)
                                          (eq? (stance-range stance) 'adjacent)
                                          (eq? (stance-range stance) 'close))
                                 (tr
                                    "sonar"
                                    "range"
                                    (symbol->string (stance-range stance))
                                  )
                                 ]
                                 [else
                                  (tr
                                    "sonar"
                                    "range"
                                    "???"
                                  )]))
       ]
      ['nose
       (append-element! body
                          (tr
                           "olfaction"
                           "range"
                           (symbol->string (stance-range stance))
                           ))
       (when (< (actor-hp actor) (actor-max-hp actor))
         (append-element! body
                          (tr
                           "olfaction"
                           "condition"
                           "wounded [HP not full, bleeding]"
                           #;(format "~a/~a" (actor-hp actor) (actor-max-hp actor))))
         )
       ]
      )
    )
  (when (not (null? (actor-statuses actor)))
    (define statuses (actor-statuses actor))
    (define statuses-list
      (tr "statuses" (~s statuses)))
    (append-element! body statuses-list))
  (info-card
   body
   name)
  )

(define (display-combat-timeline)
  (define body
    (for/list ([event (current-combat-timeline)])
      (tr (number->string (combat-event-at event)) (combat-event-details event))
      ))
  (info-card body "Combat timeline")
  )
