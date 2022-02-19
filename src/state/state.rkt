#lang at-exp racket

(provide (all-defined-out))

(provide (all-from-out "describe-situation.rkt"
                       "mutators.rkt"
                       "pending-action.rkt"
                       "resolve-counts.rkt"))

(require racket/serialize)

(require
  "../pc/pc.rkt"
  "../world/world.rkt")

(require "describe-situation.rkt"
         "mutators.rkt"
         "pending-action.rkt"
         "resolve-counts.rkt")

(define current-world (make-parameter '()))
(define current-log (make-parameter '()))
(define current-part (make-parameter 0))
(define current-chapter (make-parameter 0))
(define current-last-paragraph (make-parameter ""))
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

(define current-quests (make-parameter '()))
(define current-persistent-quests (make-parameter '()))

(define current-pc (make-parameter '()))
(define current-life (make-parameter 0))

(define current-fragment-id (make-parameter '()))

(define current-combat-timeline (make-parameter '()))

(define current-show-round-summary? (make-parameter #f))

(define (reset-situation!)
  (current-world (make-new-world))
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
  (current-quests '())
  (current-persistent-quests '())
  (current-pc (make-new-pc))
  (current-life 0)
  (current-fragment-id '())
  (current-combat-timeline '())
  (current-show-round-summary? #f)
  )


; s11n logistics are simpler when there is only one S-expression to serialize -> "wrapper" struct for global state
(serializable-struct
 state
 ([world #:mutable]
  [log #:mutable]
  [last-paragraph #:mutable]
  [part #:mutable]
  [chapter #:mutable]
  [prompt #:mutable]
  [pending-action #:mutable]
  [times-begin-traverse-narrated #:mutable]
  [times-finish-traverse-narrated #:mutable]
  [times-cancel-traverse-narrated #:mutable]
  [times-species-encountered #:mutable]
  [flags #:mutable]
  [round #:mutable]
  [run #:mutable]
  [elapsed-time #:mutable]
  [in-combat? #:mutable]
  [quests #:mutable]
  [persistent-quests #:mutable]
  [pc #:mutable]
  [life #:mutable]
  [current-fragment-id #:mutable]
  [combat-timeline #:mutable]
  [show-round-summary? #:mutable])
 #:transparent)


(define (save)
  (define output-file (open-output-file "save.txt" #:exists 'truncate)) ; truncate = delete if exists

  (define st
    (state
     (current-world)
     (current-log)
     (current-last-paragraph)
     (current-part)
     (current-chapter)
     (current-prompt)
     (current-pending-action)
     (current-times-begin-traverse-narrated)
     (current-times-finish-traverse-narrated)
     (current-times-cancel-traverse-narrated)
     (current-times-species-encountered)
     (current-flags)
     (current-round)
     (current-run)
     (current-elapsed-time)
     (current-in-combat?)
     (current-quests)
     (current-persistent-quests)
     (current-pc)
     (current-life)
     (current-fragment-id)
     (current-combat-timeline)
     (current-show-round-summary?)))

  (displayln "")
  (displayln (format "~a" st))
  (displayln "")

  (displayln "CPA:")
  (displayln (format "~a" (current-pending-action)))
  (define failing-s (serialize (current-pending-action)))
  (displayln "OK")

  (define serialized-state (serialize st))
  (write serialized-state output-file)
  (close-output-port output-file))


; NOTE: "Serialization followed by deserialization produces a value with the same graph structure and mutability as the original value, but the serialized value is a plain tree (i.e., no sharing)."
; - https://docs.racket-lang.org/reference/serialization.html
(define (load-situation-from-state serialized-state)
  (define s (deserialize serialized-state))

  (current-world (state-world s))
  (current-log (state-log s))
  (current-last-paragraph (state-last-paragraph s))
  (current-part (state-part s))
  (current-chapter (state-chapter s))
  (current-prompt (state-prompt s))
  (current-pending-action (state-pending-action s))
  (current-times-begin-traverse-narrated (state-times-begin-traverse-narrated s))
  (current-times-finish-traverse-narrated (state-times-finish-traverse-narrated s))
  (current-times-cancel-traverse-narrated (state-times-cancel-traverse-narrated s))
  (current-times-species-encountered (state-times-species-encountered s))
  (current-flags (state-flags s))
  (current-round (state-round s))
  (current-run (state-run s))
  (current-elapsed-time (state-elapsed-time s))
  (current-in-combat? (state-in-combat? s))
  (current-quests (state-quests s))
  (current-persistent-quests (state-persistent-quests s))
  (current-pc (state-pc s))
  (current-life (state-life s))
  (current-fragment-id (state-current-fragment-id s))
  (current-combat-timeline (state-combat-timeline s))
  (current-show-round-summary? (state-show-round-summary? s)))
