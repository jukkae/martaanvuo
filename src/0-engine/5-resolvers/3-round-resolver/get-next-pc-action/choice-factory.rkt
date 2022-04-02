#lang at-exp racket

(require
  "../../../1-index/content.rkt"

  "../../../2-core/io.rkt"
  "../../../2-core/core.rkt"

  "../../../3-types/action.rkt"
  "../../../3-types/actor.rkt"
  "../../../3-types/choice.rkt"
  "../../../3-types/item.rkt"
  "../../../3-types/location.rkt"
  "../../../3-types/pc-actor.rkt"
  "../../../3-types/route.rkt"
  "../../../3-types/world.rkt"


  "../../../4-rules/actors/actor.rkt"
  "../../../4-rules/blurbs/blurbs.rkt"
  "../../../4-rules/checks/checks.rkt"
  "../../../4-rules/items/item.rkt"
  "../../../4-rules/locations/locations.rkt"
  "../../../4-rules/pc/pc.rkt"
  "../../../4-rules/world/time.rkt"
  "../../../4-rules/world/world.rkt"

  "../../../6-combat/combat-pc-choices.rkt"

  "../../../7-state/state/logging.rkt"
  "../../../7-state/state/state.rkt"
  )

(provide choice-factory)
(define (choice-factory action-symbol)
  (case action-symbol
    ['sleep
     (make-choice
      'sleep
      "Sleep."
      (λ () (make-action
             #:symbol 'sleep
             #:actor (pc)
             #:duration (time-until-next-morning)
             )))]
    ['tent
     (make-choice
      'camp
      "Set up a tent."
      (λ () (make-action
             #:symbol 'camp
             #:actor (pc)
             #:duration 20
             #:resolution-rules
             `(
               (displayln "Camp action TODO")
               'ok))))]

    ['campfire
     (make-choice
      'campfire
      "Build campfire."
      (λ () (make-action
             #:symbol 'camp
             #:actor (pc)
             #:duration 10
             #:resolution-rules
             `(
               (displayln "Campfire action TODO")
               'ok))))]

    ['rest
     (define next-time-of-day
       (time-of-day-from-iotas (+ (world-elapsed-time (current-world))
                                    100)))
     (make-choice
      'rest
      (format "Rest. [until ~a]" next-time-of-day)
      (λ () (make-action
             #:symbol 'rest
             #:actor (pc)
             #:duration (time-until-next-time-of-day)
             #:resolution-rules
             `(
               (blurb 'rest-action))
             )))]

    ; submenu
    ['pick-up-item
     (make-choice
      'pick-up-item
      "Pick up an item."
      (λ ()
        (define item (select-item-to-pick-up))
        (if (or (void? item) (null? item))
            'cancel
            (begin
              (remove-item-from-location! (current-location) item)
              (add-item! item)
              (notice (format "Picked up: ~a" (item-name item)))
              (make-action
               #:symbol 'pick-up
               #:actor (pc)
               #:duration 1 ; should be "negligible", aka 0.1 iota
               #:resolution-rules '()
               )
              ))
        ))
     ]

    ; This opens a submenu
    ['eat
     (define title
       (case (pc-hunger-level)
         ['satiated "Eat? Disgusting idea."]
         ['not-hungry "Not really hungry but she could eat."]
         ['hungry "Eat."]
         ['very-hungry "Eat, she's very hungry."]
         ['starving "Eat. She's starving, eat. Eat now."]
         ))
     (make-choice
      'eat
      "Eat."
      (λ ()
        (define food (select-food-to-eat))
        (if (or (void? food) (null? food))
            'cancel
            (begin
              (make-action
               #:symbol 'eat
               #:actor (pc)
               #:duration 15
               #:target food
               #:tags '(downtime)
               #:resolution-rules
               `(
                 (define id ',(item-id food))
                 (define food-tier
                   (case id
                     ['fresh-berries 0]
                     ['berries 0]
                     ['decaying-berries 0]
                     ['ration 1]
                     ['vatruska 2]
                     [else 1])
                   )
                 (decrease-pc-hunger-level food-tier)
                 (when (eq? id 'decaying-berries)
                  (actor-add-condition! (pc) (condition 'food-poisoning "Food poisoning" '()))
                 )

                ;  (case ',food-id
                ;    ['fresh-berries (p "The berries are invigoratingly sweet.")]
                ;    ['ration (p "The ration's dry and bland, but filling.")]
                ;    ['vatruska (p "The vatruska tastes heavenly.")])
                 (remove-item! id)

                 ))))

        ))
     ]
    ))

(define (select-food-to-eat)
  (define items (actor-inventory (pc)))
  (define comestibles
    (filter (λ (item) ; likely this should be stored as data on the item itself
              (case (item-id item)
                ['fresh-berries #t]
                ['berries #t]
                ['decaying-berries #t]
                ['ration #t]
                ['vatruska #t]
                [else #f]))
            items))

  (prln (format "Eat what? [1-~a], anything else to cancel." (length comestibles)))
  (br)

  (for ([food comestibles]
        [i (in-naturals 1)])
    (prln (format "[~a] ~a (~a)" i (item-name food) (item-quantity food))))
  (br)
  (define input (string->number (wait-for-input)))
  (cond ((and (number? input)
              (> input 0)
              (<= input (length comestibles)))
         (define index (- input 1))
         (list-ref comestibles index)
         )
        (else '()#;(p "Nevermind.")))
  )

(define (select-item-to-pick-up)
  (define items (location-items (current-location)))

  (prln (format "Pick up what? [1-~a], anything else to cancel." (length items)))
  (br)

  (for ([item items]
        [i (in-naturals 1)])
    (if (= (item-quantity item) 1)
        (prln (format "[~a] ~a" i (item-name item)))
        (prln (format "[~a] ~a (~a)" i (item-name item) (item-quantity item))) ; TODO: pluralized
        )
    )
  (br)
  (define input (string->number (wait-for-input)))
  (cond ((and (number? input)
              (> input 0)
              (<= input (length items)))
         (define index (- input 1))
         (list-ref items index)
         )
        (else '()#;(p "Nevermind.")))
  )