#lang at-exp racket

(provide (all-defined-out))

(require racket/lazy-require)

(require
  "combat-event.rkt"
  "narration.rkt"
  "stance.rkt"

  "../2-core/io.rkt"
  "../2-core/core.rkt"

  "../3-types/actor.rkt"
  "../3-types/pc-actor.rkt"
  "../3-types/status.rkt"
  "../3-types/condition.rkt"
  "../3-types/world.rkt"

  "../4-rules/actors/actor.rkt"
  "../4-rules/pc/pc.rkt"

  "../7-state/session/session.rkt"
  )

(lazy-require ["../5-resolvers/round-resolver/round-resolver.rkt"
  (go-to-fragment
   )])

(lazy-require ["../7-state/state/state.rkt"
  (current-in-combat?
   current-log
   current-location
   current-combat-timeline
   current-world
   get-current-enemies
   pc
   )])

(lazy-require ["../7-state/state/logging.rkt"
  (next-chapter!
   )])


(define (make-combat-event details)
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
  (current-session-score-dice++ (if (= (current-session-times-in-combat) 1) "Got in combat." '()))
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
  (when (pc-is-alive?)
    #;(go-to-fragment 'post-combat)
    (next-chapter!)))
