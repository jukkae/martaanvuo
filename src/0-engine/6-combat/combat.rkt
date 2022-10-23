#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require

  "../1-index/state.rkt"

  "../2-core/io.rkt"
  "../2-core/core.rkt"

  "../3-types/actor.rkt"
  "../3-types/combat-event.rkt"
  "../3-types/condition.rkt"
  "../3-types/pc-actor.rkt"
  "../3-types/stance.rkt"
  "../3-types/status.rkt"
  "../3-types/world.rkt"

  "../4-systems/actors/actor.rkt"
  "../4-systems/pc/pc.rkt"

  "../../1-content/narration/combat-narration.rkt"
  )

(define (make-combat-event details)
  (combat-event* details (world-elapsed-time (current-world))))

(define (add-combat-event text)
  (current-combat-timeline (append-element (current-combat-timeline) (make-combat-event text))))

(define (begin-combat!)
  (current-in-combat? #t)
  ; TODO: move this kind of stuff to content
  (current-session-times-in-combat++)
  (current-session-score-dice++ (if (= (current-session-times-in-combat) 1) "got in combat" '()))
  ;   (notice "Attainment: Fighter"))
  (add-combat-event "combat started"))

(define (end-combat!)
  (define success-text (if (pc-is-alive?) "Otava survived." "Otava died."))
  (notice (format "Combat finished. [~a]" success-text))
  (add-combat-event "combat finished")
  (display-combat-timeline)
  (current-in-combat? #f)
  (current-combat-timeline '())
  (wait-for-confirm)
  #;(when (pc-is-alive?)
    #;(go-to-fragment 'post-combat)
    #;(next-chapter!)))
