#lang at-exp racket

(provide (all-defined-out))

(require "../actors/actor.rkt"
         "../items/item.rkt"

         "../../1-index/state.rkt"

         "../../2-core/io.rkt"
         "../../2-core/core.rkt"

         "../../3-types/actor.rkt"
         "../../3-types/item.rkt"
         "../../3-types/modification.rkt"
         "../../3-types/sense-organ.rkt"
         "../../3-types/manipulator.rkt"
         "../../3-types/pc-actor.rkt")

; PC hp <= 0 starts a "dying" counter of 1-2 rounds
; - this as an "upgrade" or talent or build thing or whatever
; - or a saving throw on any damage
(define (make-new-pc)
  (make-pc-actor "Otava"
                 #:max-hp 6
                 #:max-lp 4
                 #:size 'large
                 #:sense-organs (list (SenseOrgan 'basic-homeostasis 1 "basic homeostasis")
                                      (SenseOrgan 'nociception 1 "nociception")
                                      ;(SenseOrgan 'nose 1 "nose")
                                      (SenseOrgan 'eyes 1 "eyes")
                                      (SenseOrgan 'ears 1 "ears")
                                      ;(SenseOrgan 'sonar 1 "sonar")
                                      #;(SenseOrgan 'haptics 1 "haptics"))
                 #:manipulators (list (Manipulator 'human-hands "hands at end of arms")
                                      (Manipulator 'legs "feet at end of legs")
                                      (Manipulator 'mouth "speech-capable mouth")
                                      ; (Manipulator 'nimble-hands "nimble hands")
                                      ; (Manipulator 'legs "legs")
                                      ; (Manipulator 'wings "wings")
                                      ; (Manipulator 'prehensile-feet "prehensile feet")
                                      ; (Manipulator 'sharp-incisors "sharp incisors")
                                      )))

; TODO: refactor!
(define (make-new-mb-pc)
  (make-pc-actor "Otava"
                 #:max-hp 6
                 #:max-lp 4
                 #:size 'large
                 #:sense-organs (list (SenseOrgan 'basic-homeostasis 1 "basic homeostasis")
                                      (SenseOrgan 'nociception 1 "nociception")
                                      (SenseOrgan 'eyes 1 "eyes")
                                      (SenseOrgan 'haptics 1 "haptics"))
                 #:manipulators (list (Manipulator 'human-hands "hands at end of arms")
                                      (Manipulator 'legs "feet at end of legs")
                                      ; (Manipulator 'nimble-hands "nimble hands")
                                      ; (Manipulator 'legs "legs")
                                      ; (Manipulator 'wings "wings")
                                      ; (Manipulator 'prehensile-feet "prehensile feet")
                                      ; (Manipulator 'sharp-incisors "sharp incisors")
                                      )))

(define (set-base-build!)
  (set-actor-strength! (pc) 7)
  (set-actor-dexterity! (pc) 10)
  (set-actor-constitution! (pc) 10)
  (set-actor-intelligence! (pc) 7)
  (set-actor-charisma! (pc) 7)

  (set-pc-actor-max-lp! (pc) 1)
  (set-pc-actor-lp! (pc) 1)
  (set-pc-actor-hunger! (pc) 350)
  (set-pc-actor-fatigue! (pc) 0)

  ; (set-trait! (pc) "athletics-skill" 1)
  (set-trait! (pc) "melee-attack-skill" 3)
  ; (set-trait! (pc) "wrestle-attack-skill" -1)
  (set-trait! (pc) "defense" 1)
  ; (set-trait! (pc) "exploration-skill" 1)

  ; (add-item! 'knife #:amount 2 #:silent? #t)
  ; (add-item! 'ration #:amount 2 #:silent? #t)
  (add-item! 'bolt-cutters #:silent? #t)
  (add-item! 'empty-flashlight #:silent? #t)
  (add-item! 'battery #:silent? #t)
  (add-item! 'lucky-charm-slot-machine #:silent? #t)
  ; (add-item! 'revolver #:silent? #t)
  )

(define (set-markbearer-build!)
  (set-actor-strength! (pc) 7)
  (set-actor-dexterity! (pc) 10)
  (set-actor-constitution! (pc) 10)
  (set-actor-intelligence! (pc) 7)
  (set-actor-charisma! (pc) 7)

  (set-pc-actor-max-lp! (pc) 1)
  (set-pc-actor-lp! (pc) 1)
  (set-pc-actor-hunger! (pc) 350)
  (set-pc-actor-fatigue! (pc) 0)

  ; (set-trait! (pc) "athletics-skill" 1)
  (set-trait! (pc) "melee-attack-skill" 3)
  ; (set-trait! (pc) "wrestle-attack-skill" -1)
  (set-trait! (pc) "defense" 1)
  ; (set-trait! (pc) "exploration-skill" 1)

  ; (add-item! 'knife #:amount 2 #:silent? #t)
  ; (add-item! 'ration #:amount 2 #:silent? #t)
  (add-item! 'bolt-cutters #:silent? #t)
  (add-item! 'lucky-charm-slot-machine #:silent? #t))

; (: -> Modification '())
(define (add-modification! modification)
  (define old-modifications (pc-actor-modifications (pc)))
  (set-pc-actor-modifications! (pc) (append-element old-modifications modification))
  (modification-info-card modification #:title "Modification installed"))

; (: -> modification-id '())
(define (remove-modification! id)
  (define modifications (pc-actor-modifications (pc)))
  (define it (findf (λ (modification) (equal? (Modification-id modification) id)) modifications))
  (cond
    [it
     (set-pc-actor-modifications!
      (pc)
      (filter (λ (modification) (not (equal? (Modification-id modification) id)))
              (actor-inventory (pc))))
     (dev-note "modification removed, show info about removed/remaining modifications")]))

; (: -> (U Item Symbol) Natural String Boolean '())
(define (add-item! item #:amount [amount 1] #:title [title "Item added"] #:silent? [silent? #f])
  (define actor (pc))
  (when (symbol? item)
    (set! item (make-item item #:amount amount)))
  (add-item-to-inventory! actor item)
  (when (not silent?)
    (item-info-card item #:title title)))

(define (remove-item! id #:quantity-to-remove [quantity-to-remove 1])
  (define items (actor-inventory (pc)))
  (define it (findf (λ (inventory-item) (equal? (item-id inventory-item) id)) items))
  (cond
    [it
     (cond
       [(equal? quantity-to-remove 'all)
        (set-actor-inventory! (pc)
                              (filter (λ (inventory-item) (not (equal? (item-id inventory-item) id)))
                                      (actor-inventory (pc))))]
       [else
        (if (> (item-quantity it) 1)
            (set-item-quantity! it (- (item-quantity it) 1))
            (set-actor-inventory!
             (pc)
             (filter (λ (inventory-item) (not (equal? (item-id inventory-item) id)))
                     (actor-inventory (pc)))))])
      (notice (format "removed x~a: ~a" quantity-to-remove (item-name it)))
     ]))

(define (add-ammo! amount)
  (define items (actor-inventory (pc)))
  (define revolver (findf (λ (inventory-item) (equal? (item-id inventory-item) 'revolver)) items))
  (increase-ammo! revolver))

(define (consume-ammo! amount)
  (define items (actor-inventory (pc)))
  (define revolver (findf (λ (inventory-item) (equal? (item-id inventory-item) 'revolver)) items))
  (cond
    [revolver (set-ranged-weapon-ammo-left! revolver (sub1 (ranged-weapon-ammo-left revolver)))]))

(define (pc-has-comestibles?)
  (define items (actor-inventory (pc)))
  (define comestibles
    (filter (λ (item) ; likely this should be stored as data on the item itself
              (case (item-id item)
                ['fresh-berries #t]
                ['berries #t]
                ['decaying-berries #t]
                ['ration #t]
                ['vatruska #t]
                ['salmon #t]
                ['decaying-salmon #t]
                [else #f]))
            items))
  (if (not (null? comestibles))
    #t
    #f))

(define (pc-has-item? id)
  (define items (actor-inventory (pc)))
  (findf (λ (inventory-item) (equal? (item-id inventory-item) id)) items))

(define (pc-has-sense-organ? id [level '()])
  (define sense-organs (pc-actor-sense-organs (pc)))
  (cond
    [(not (null? level))
     (findf (λ (sense-organ)
              (and (equal? (SenseOrgan-id sense-organ) id) (>= (SenseOrgan-level sense-organ) level)))
            sense-organs)]
    [else (findf (λ (sense-organ) (equal? (SenseOrgan-id sense-organ) id)) sense-organs)]))

; (: -> SenseOrgan '())
(define (add-sense-organ! sense-organ [silent? #f])
  (define old-sense-organs (pc-actor-sense-organs (pc)))
  (set-pc-actor-sense-organs! (pc) (append-element old-sense-organs sense-organ))
  (when (not silent?)
    (notice (format "Sense organ added: ~a" (SenseOrgan-name sense-organ)))))

; (: -> SenseOrgan-id '())
(define (remove-sense-organ! id)
  (define sense-organs (pc-actor-sense-organs (pc)))
  (define it (findf (λ (sense-organ) (equal? (SenseOrgan-id sense-organ) id)) sense-organs))
  (cond
    [it
     (set-pc-actor-sense-organs!
      (pc)
      (filter (λ (sense-organ) (not (equal? (SenseOrgan-id sense-organ) id)))
              (pc-actor-sense-organs (pc))))
     (notice (format "Sense organ removed: ~a" (SenseOrgan-name it)))]))

(define (remove-all-sense-organs!)
  (define sense-organs (pc-actor-sense-organs (pc)))
  (for ([sense-organ sense-organs])
    (remove-sense-organ! (SenseOrgan-id sense-organ)))
  (notice "Otava is an existence cut off completely from the outside world."))

(define (pc-has-manipulator? id)
  (define manipulators (pc-actor-manipulators (pc)))
  (findf (λ (manipulator) (equal? (Manipulator-id manipulator) id)) manipulators))

; (: -> Manipulator '())
(define (add-manipulator! manipulator [silent? #f])
  (define old-manipulators (pc-actor-manipulators (pc)))
  (set-pc-actor-manipulators! (pc) (append-element old-manipulators manipulator))
  (when (not silent?)
    (notice (format "Manipulator added: ~a" (Manipulator-name manipulator)))))

; (: -> Manipulator-id '())
(define (remove-manipulator! id)
  (define manipulators (pc-actor-manipulators (pc)))
  (define it (findf (λ (manipulator) (equal? (Manipulator-id manipulator) id)) manipulators))
  (cond
    [it
     (set-pc-actor-manipulators!
      (pc)
      (filter (λ (manipulator) (not (equal? (Manipulator-id manipulator) id)))
              (pc-actor-manipulators (pc))))
     (notice (format "Manipulator removed: ~a" (Manipulator-name it)))]))

(define (pc-gold-amount)
  (define items (actor-inventory (pc)))
  (define gold? (findf (λ (inventory-item) (equal? (item-id inventory-item) 'gold)) items))
  (if gold? (item-quantity gold?) 0))

(define (pc-has-money amount)
  (>= (pc-gold-amount) amount))

(define (decrease-pc-money! quantity)
  (define items (actor-inventory (pc)))
  (define gold? (findf (λ (inventory-item) (equal? (item-id inventory-item) 'gold)) items))
  (cond
    [gold? (set-item-quantity! gold? (- (item-quantity gold?) quantity))])
  (notice (format "Gold decreased by ~a, new quantity: ~a" quantity (item-quantity gold?))))

; TODO: combine
(define (increase-pc-money! quantity)
  (define items (actor-inventory (pc)))
  (define gold? (findf (λ (inventory-item) (equal? (item-id inventory-item) 'gold)) items))
  (cond
    [(not gold?)
     (add-item! 'gold #:amount quantity)
     (notice (format "Otava now has ~a gold" quantity))]
    [else
     (set-item-quantity! gold? (+ (item-quantity gold?) quantity))
     (notice (format "Gold increased by ~a, new quantity: ~a" quantity (item-quantity gold?)))]))

(define (pc-has-ammo-left?)
  (define items (actor-inventory (pc)))
  (define revolver (findf (λ (inventory-item) (equal? (item-id inventory-item) 'revolver)) items))
  (cond
    [revolver
     (define ammo-left (ranged-weapon-ammo-left revolver))
     (positive? ammo-left)]
    [else #f]))

(define (pc-hungry?)
  (or (equal? (pc-hunger-level) 'hungry)
      (equal? (pc-hunger-level) 'very-hungry)
      (equal? (pc-hunger-level) 'starving)))

(define hunger-level-hungry 400)
(define hunger-level-very-hungry 800)
(define hunger-level-starving 1600)

; a day is 600 ticks -> if you eat always when you get hungry, once a day is not enough
(define (decrease-pc-hunger-level levels)
  (let ([current-hunger (pc-hunger-level)])
    (case current-hunger
      ['satiated (set-pc-actor-hunger! (pc) 0)]
      ['not-hungry (set-pc-actor-hunger! (pc) 0)]
      ['hungry
       (case levels
         [(0) (set-pc-actor-hunger! (pc) hunger-level-hungry)]
         [(1) (set-pc-actor-hunger! (pc) 0)]
         [(2) (set-pc-actor-hunger! (pc) 0)])]
      ['very-hungry
       (case levels
         [(0) (set-pc-actor-hunger! (pc) hunger-level-very-hungry)]
         [(1) (set-pc-actor-hunger! (pc) hunger-level-hungry)]
         [(2) (set-pc-actor-hunger! (pc) 0)])]
      ['starving
       (case levels
         [(0) (set-pc-actor-hunger! (pc) hunger-level-starving)]
         [(1) (set-pc-actor-hunger! (pc) hunger-level-very-hungry)]
         [(2) (set-pc-actor-hunger! (pc) hunger-level-hungry)])])

    (when (and (or (equal? current-hunger 'hungry)
                   (equal? current-hunger 'very-hungry)
                   (equal? current-hunger 'starving))
               (= levels 2))
      (p "Damn that tastes good.")
      (award-xp! 3))

    (define hunger-string
      (case (pc-hunger-level)
        ['satiated "satiated"]
        ['not-hungry "not hungry anymore"]
        ['hungry "less hungry"]
        ['very-hungry "still very hungry"]
        ['starving "still starving"]))
    (notice (format "Otava is now ~a." hunger-string))))

(define (pc-hunger-level)
  (cond
    [(<= (pc-actor-hunger (pc)) 50) 'satiated]
    [(< (pc-actor-hunger (pc)) hunger-level-hungry) 'not-hungry]
    [(< (pc-actor-hunger (pc)) hunger-level-very-hungry) 'hungry]
    [(< (pc-actor-hunger (pc)) hunger-level-starving) 'very-hungry]
    [else 'starving]))

(define (pc-fatigue-level)
  (cond
    [(<= (pc-actor-fatigue (pc)) 500) 'not-tired]
    [(< (pc-actor-fatigue (pc)) 1000) 'tired]
    [(< (pc-actor-hunger (pc) 1500)) 'drained]
    [else 'exhausted]))

(define (pc-is-alive?)
  (case (pc-actor-alive? (pc))
    ['dead #f]
    [else (actor-alive? (pc))]))

(define (kill-pc! cause-of-death)
  (clear-current-fragment!)
  (narrate-pc-death cause-of-death)
  (kill (pc) cause-of-death))

(define (narrate-pc-death cause-of-death)
  (case cause-of-death
    ['fell-to-death
     (p
      "Otava's mangled body hangs limp against the rock wall, a feast for carrioneaters, torn apart, sustenance for new generations, possessions strewn in the underbrush.")]))

(provide award-xp!)
(define (award-xp! amount [reason '()])
  (if (null? reason) (notice (format "+~a xp" amount)) (notice (format "+~a xp ~a" amount reason)))
  (set-pc-actor-xp! (pc) (+ (pc-actor-xp (pc)) amount)))

(define (pc-has-condition-of-type? t)
  (actor-has-condition-of-type? (pc) t))

(define (pc-envenomed-peaking?)
  (and (actor-has-condition-of-type? (pc) 'envenomed)
       (>= (condition-age (actor-get-condition-of-type (pc) 'envenomed)) 5)
       (< (condition-age (actor-get-condition-of-type (pc) 'envenomed)) 100)))

(define sense-organs
  (list (SenseOrgan 'basic-homeostasis 1 "basic homeostasis")
        (SenseOrgan 'nociception 1 "nociception")
        (SenseOrgan 'nose 1 "nose")
        (SenseOrgan 'eyes 1 "eyes")
        (SenseOrgan 'ears 1 "ears")
        (SenseOrgan 'sonar 1 "sonar")
        (SenseOrgan 'haptics 1 "haptics")))

(define (randomize-pc-senses!)
  (define roll (d 1 6))
  (notice (format "Gift roll (1d6): [~a]" roll))
  (remove-all-sense-organs!)
  (for ([sense-organ (take-n-random sense-organs roll)])
    (add-sense-organ! sense-organ)))

(define (display-pc-title)
  (notice (format "Otava ~a" (string-append* (add-between (current-epithets) ", ")))))

(define (pc-has-light-source?)
  (pc-has-item? 'flashlight-on)
  (pc-has-item? 'torch)
  (pc-has-manipulator? 'bioluminescence))
