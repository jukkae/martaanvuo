#lang at-exp racket


(provide (all-defined-out))

(require racket/lazy-require)

(require
  "../actors/actor.rkt"
  "../actors/pc-actor.rkt"

  "../core/io.rkt"
  "../core/utils.rkt"

  "../locations/0-types/location.rkt"

  "../pc/character-sheet.rkt"

  "../quests/quest.rkt"

  "../combat/stance.rkt")

(lazy-require
 ["state.rkt" (current-flags
               current-fragment-id
               current-in-combat?
               current-life
               current-pc
               current-quests)])

(lazy-require
 ["../combat/combat.rkt" (end-combat!)])

;; bad name for this file, this is sort of "misc"

(define (set-flag flag)
  (when (not (flag-set? flag))
    (current-flags (append-element (current-flags) flag))))

(define (remove-flag flag)
  (when (flag-set? flag)
    (current-flags (remq flag (current-flags)))))

(define (flag-set? flag)
  (memq flag (current-flags)))

(define (print-flags)
  (dev-note "print-flags:")
  (displayln (current-flags)))


(define (in-combat?) (current-in-combat?))

; API
(define (engaged?)
  (define any-enemy-engaged? #f)

  (for ([enemy (get-current-enemies)])
    (let ([stance (actor-stance enemy)])
      (when (eq? (stance-range stance) 'engaged)
        (set! any-enemy-engaged? #t))))

  any-enemy-engaged?)

; API
(define (get-an-enemy-at-range range)
  (define current-enemies (get-current-enemies))
  (define enemies-shuffled (shuffle current-enemies))
  (define enemy-in-range '())
  (for ([enemy enemies-shuffled])
    (define stance (actor-stance enemy))
    (when (eq? (stance-range stance) range)
      (set! enemy-in-range enemy)))
  enemy-in-range)

; API
(define (get-enemies-at-range range)
  (define current-enemies (get-current-enemies))
  (define enemies-in-range '())
  (for ([enemy current-enemies])
    (define stance (actor-stance enemy))
    (when (eq? (stance-range stance) range)
      (set! enemies-in-range (append-element enemies-in-range enemy))))
  enemies-in-range)

; API
(define (in-range? target attack-mode)
  (case attack-mode
    ['melee
     (dev-note "in-range? tbd")
     #t]
    [else
     (dev-note "in-range? tbd")
     #f]))

; api
(define (current-location)
  (actor-location (pc)))


; api
(define (get-current-enemies)
  (filter
   (λ (actor) (and (actor-alive? actor)
                   (not (pc-actor? actor))))
   (location-actors (current-location))))

; api
(define (quests)
  (current-quests))

(define (find-quest id)
  (findf (λ (q) (eq? (quest-id q) id))
         (current-quests)))



(define (reduce-debt-by! amount)
  (define debt-quest (find-quest 'pay-off-debt))

  (define old-debt-amount (quest-details debt-quest))
  (define new-debt-amount (- old-debt-amount amount))
  (set-quest-details! debt-quest new-debt-amount)
  (set-quest-notes! debt-quest
                    (string-append
                     "unsettled: "
                     (number->string new-debt-amount)
                     " g gold"))
  (displayln "new-debt-amount:")
  #;(displayln (~r new-debt-amount)) ; formatting todo
  (displayln new-debt-amount)

  '())


; scripting API / situation
(provide pc)
(define (pc)
  (current-pc))

; scripting API / situation / implementation detail
(define (remove-all-enemies-and-end-combat!)
  (for ([enemy (get-current-enemies)])
    (remove-actor-from-location! (actor-location enemy) enemy))
  (end-combat!)
  (dev-note "post-combat steps")) ; for instance, wound care (fast vs good), xp, summary etc


; scripting API
(define (remove-enemy enemy)
  (remove-actor-from-location! (actor-location enemy) enemy))

; scripting API
(provide actor-in-range?)
(define (actor-in-range? enemy range)
  (define stance (actor-stance enemy))
  (eq? (stance-range stance) range))


; scripting API?
(define (player-info)
  (define player-status
    (list
     (list " life " (string-append " " (number->string (current-life)) " "))))

  (info-card player-status (string-append "Player status")))

; api?
(define (pick-up-items!)
  (p "Otava picks up everything there is to pick up.")
  (define all-items (location-items (current-location)))
  (for ([item all-items])
    (remove-item-from-location! (current-location) item)
    (add-item-to-inventory! (pc) item))
  (inventory))

(define (add-quest! quest)
  (current-quests
   (append-element (current-quests) quest)))

(define (quest-exists? id)
  (define quests (current-quests))
  (if (null? quests)
      #f
      (findf (λ (quest) (eq? id (quest-id quest))) quests)))

; set-quest-status! is obviously reserved
(define (update-quest-status! id status)
  (define quests (current-quests))
  (define quest (findf (λ (quest) (eq? id (quest-id quest))) quests))
  (when quest
    (set-quest-status! quest status)))

; would be nice to add instead of overwrite, but that requires smart linebreaking in info-cards
(define (update-quest-notes! id notes)
  (define quests (current-quests))
  (define quest (findf (λ (quest) (eq? id (quest-id quest))) quests))
  (when quest
    (set-quest-notes! quest notes)))

(define (update-quest-details! id details)
  (define quests (current-quests))
  (define quest (findf (λ (quest) (eq? id (quest-id quest))) quests))
  (when quest
    (set-quest-details! quest details)))

(define (increment-achievement! achievement)
  (case achievement
    ['forgetful (displayln "forgetful achievement incremented")]
    [else
     (displayln "increment-achievement!: unknown achievement:")
     (displayln achievement)]))

(define (unset-current-fragment-id!)
  (current-fragment-id '()))
