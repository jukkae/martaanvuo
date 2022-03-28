#lang at-exp racket

(provide (all-defined-out))

(provide (all-from-out
          "0-types/state.rkt"
          "describe-situation.rkt"
          "mutators.rkt"
          "pending-action.rkt"
          "resolve-counts.rkt"))

(require racket/serialize)

(require
  "0-types/state.rkt"
  "../pc/pc.rkt"
  "../world/world.rkt")

(require "describe-situation.rkt"
         "mutators.rkt"
         "pending-action.rkt"
         "resolve-counts.rkt")

(define current-world (make-parameter '()))

; Where does this belong?
(define current-last-numeric-actor-id (make-parameter 0))
(define (current-last-numeric-actor-id++)
  (current-last-numeric-actor-id (add1 (current-last-numeric-actor-id))))
(define (get-next-numeric-actor-id)
  (current-last-numeric-actor-id++)
  (current-last-numeric-actor-id))

(define current-log (make-parameter '()))
(define current-last-paragraph (make-parameter ""))
(define current-part (make-parameter 0))
(define current-chapter (make-parameter 0))
(define current-prompt (make-parameter ""))

(define current-pending-action (make-parameter '()))

(define current-times-begin-traverse-narrated (make-parameter (make-hash)))
(define current-times-finish-traverse-narrated (make-parameter (make-hash)))
(define current-times-cancel-traverse-narrated (make-parameter (make-hash)))
(define current-times-species-encountered (make-parameter (make-hash)))

(define (current-times-species-encountered++ species)
  (hash-set! (current-times-species-encountered)
             species
             (add1 (hash-ref (current-times-species-encountered)
                             species
                             0))))

(define current-flags (make-parameter '()))

(define current-round (make-parameter 0))
(define current-run (make-parameter 0))
(define current-elapsed-time (make-parameter 0))

(define current-in-combat? (make-parameter #f))

(define current-tasks (make-parameter '()))

(define current-pc (make-parameter '()))
(define current-life (make-parameter 0))

(define current-fragment-id (make-parameter '()))
(define current-completed-fragments (make-parameter '()))

(define current-combat-timeline (make-parameter '()))

(define current-show-round-summary? (make-parameter #f))

(define (reset-situation!)
  (current-world (make-new-world))
  (current-last-numeric-actor-id 0)
  (current-log '())
  (current-last-paragraph "")
  (current-part 0)
  (current-chapter 0)
  (current-prompt "")
  (current-pending-action '())
  (current-times-begin-traverse-narrated (make-hash))
  (current-times-finish-traverse-narrated (make-hash))
  (current-times-cancel-traverse-narrated (make-hash))
  (current-times-species-encountered (make-hash))
  (current-flags '())
  (current-round 0)
  (current-run 0)
  (current-elapsed-time 0)
  (current-in-combat? #f)
  (current-tasks '())
  (current-pc (make-new-pc))
  (current-life 0)
  (current-fragment-id '())
  (current-completed-fragments '())
  (current-combat-timeline '())
  (current-show-round-summary? #f)
  )

(define (save)
  (define st
    (State
     (current-world) ; world
     (current-last-numeric-actor-id) ; Natural
     (current-log) ; String?
     (current-last-paragraph) ; String?
     (current-part) ; Natural
     (current-chapter) ; Natural
     (current-prompt) ; String?
     (current-pending-action) ; action
     (current-times-begin-traverse-narrated) ; Natural
     (current-times-finish-traverse-narrated) ; Natural
     (current-times-cancel-traverse-narrated) ; Natural
     (current-times-species-encountered) ; Natural
     (current-flags) ; (Listof Symbol)
     (current-round) ; Natural
     (current-run) ; Natural
     (current-elapsed-time) ; Natural, should be in-world timestamp
     (current-in-combat?) ; Boolean
     (current-tasks) ; (Listof task)
     (current-pc) ; pc-actor
     (current-life) ; Natural
     (current-fragment-id) ; Symbol
     (current-completed-fragments) ; (Listof Symbol)
     (current-combat-timeline) ; timeline
     (current-show-round-summary?) ; Boolean
     ))

  (define serialized-state (serialize st))

  (define output-file (open-output-file "save.txt" #:exists 'truncate)) ; truncate = delete if exists
  (write serialized-state output-file)
  (close-output-port output-file))


; NOTE: "Serialization followed by deserialization produces a value with the same graph structure and mutability as the original value, but the serialized value is a plain tree (i.e., no sharing)."
; - https://docs.racket-lang.org/reference/serialization.html
(define (load-situation-from-state serialized-state)
  (define s (deserialize serialized-state))

  (current-world (State-world s))
  (current-last-numeric-actor-id (State-last-numeric-actor-id s))
  (current-log (State-log s))
  (current-last-paragraph (State-last-paragraph s))
  (current-part (State-part s))
  (current-chapter (State-chapter s))
  (current-prompt (State-prompt s))
  (current-pending-action (State-pending-action s))
  (current-times-begin-traverse-narrated (State-times-begin-traverse-narrated s))
  (current-times-finish-traverse-narrated (State-times-finish-traverse-narrated s))
  (current-times-cancel-traverse-narrated (State-times-cancel-traverse-narrated s))
  (current-times-species-encountered (State-times-species-encountered s))
  (current-flags (State-flags s))
  (current-round (State-round s))
  (current-run (State-run s))
  (current-elapsed-time (State-elapsed-time s))
  (current-in-combat? (State-in-combat? s))
  (current-tasks (State-tasks s))
  (current-pc (State-pc s))
  (current-life (State-life s))
  (current-fragment-id (State-current-fragment-id s))
  (current-completed-fragments (State-completed-fragments s))
  (current-combat-timeline (State-combat-timeline s))
  (current-show-round-summary? (State-show-round-summary? s)))