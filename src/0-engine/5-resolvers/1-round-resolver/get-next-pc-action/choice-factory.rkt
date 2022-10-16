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
  "../../../3-types/name.rkt"
  "../../../3-types/place.rkt"
  "../../../3-types/pc-actor.rkt"
  "../../../3-types/route.rkt"
  "../../../3-types/world.rkt"


  "../../../4-systems/actors/actor.rkt"
  "../../../4-systems/blurbs/blurbs.rkt"
  "../../../4-systems/checks/checks.rkt"
  "../../../4-systems/items/item.rkt"
  "../../../4-systems/locations/locations.rkt"
  "../../../4-systems/pc/pc.rkt"
  "../../../4-systems/world/time.rkt"
  "../../../4-systems/world/world.rkt"

  "../../../6-combat/combat-pc-choices.rkt"

  "../../../7-state/logging.rkt"
  "../../../7-state/state.rkt"
  )

; TODO: action-symbol to choice mapping belongs to content
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
    #;['tent
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

    #;['campfire
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
             #:on-before-rules
             `(
               (blurb 'rest-action)
               (define bonus 4)
               (define bonus-str " + 4")
               (define encounter-roll (+ (d 1 6) bonus))
               (define tn 5)
               (notice (format "Encounter roll: 1d6~a >= ~a: [~a] – ~a"
                               bonus-str
                               tn
                               encounter-roll
                               (if (>= encounter-roll tn)
                                   "success"
                                   "fail")))
               (cond [(< encounter-roll tn)
                      (define resolve-events
                        (list
                         (make-event ,''spawn-encounter
                                     '() ; pack info about enemies / event here
                                     #:interrupting? #t)))
                      (define metadata '(interrupted))
                      (define duration
                        (exact-floor (/
                                      (time-until-next-time-of-day)
                                      3)))

                      (define world-tl (advance-time-until-next-interesting-event! duration #f))
                      (define world-events (timeline-events world-tl))

                      (define all-events (append world-events resolve-events))
                      (define all-metadata (append (timeline-metadata world-tl) metadata))

                      (define tl (timeline all-metadata all-events duration))

                      (process-timeline! tl)
                      tl]
                     [else 'ok])
               )
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
              (make-action
               #:symbol 'pick-up
               #:actor (pc)
               #:duration 1
               #:tags (if (current-in-combat?)
                          '(initiative-based-resolution)
                          '())
               #:resolution-rules
               `(
                ;  (define the-item ,item)
                ;  (define id ',(item-id item))
                 (define item-id ',(item-id item))
                 (define the-item (find-item-in-current-zone item-id))
                 (remove-interactible-from-current-zone! the-item)
                 (add-item! the-item)
                 (notice (format "Picked up: ~a"
                                 (cond [(item? the-item) (item-name the-item)]
                                       [else (format "~a" the-item)])))
                ;  (displayln the-item)
                ;  (notice (format "Picking up: ~a" "foo"))
                 )
               )
              ))
        )
      #:available-in-combat? #t)
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
                 (when (equal? id 'decaying-berries)
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
  (define items (current-zone-items (current-location)))

  (prln (format "Pick up what? [1-~a], anything else to cancel." (length items)))
  (br)

  (for ([item items]
        [i (in-naturals 1)])
    (define name
      (cond [(item? item)
             (cond [(Name? (item-name item))
                    (format "The ~a" (Name-singular (item-name item)))
                   ]
                   [(string? (item-name item))
                    (item-name item)])]
            [else (format "~a" item)]))
    (cond [(item? item)
           (if (= (item-quantity item) 1)
               (prln (format "[~a] ~a." i name))
               (prln (format "[~a] ~a (~a)." i name (item-quantity item))) ; TODO: pluralized
               )
           ]
          [else (prln (format "[~a] ~a." i name))])
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
