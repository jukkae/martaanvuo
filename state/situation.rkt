#lang racket

(provide (all-defined-out))

(provide (all-from-out "describe-situation.rkt"
                       "mutators.rkt"
                       "pending-action.rkt"
                       "resolve-counts.rkt"))

(require racket/lazy-require)
(require racket/serialize)

(require "../action.rkt")
(require "../actor.rkt")
(require "../condition.rkt")
(require "../io.rkt")
(require "../location.rkt")
(require "../pc.rkt")
(require "../place.rkt")
(require "../quest.rkt")
(require "../route.rkt")
(require "../stance.rkt")
(require "../status.rkt")
(require "../utils.rkt")
(require "../world.rkt")

(require "combat.rkt"
         "describe-situation.rkt"
         "logging.rkt"
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


(define (reset-situation!)
  (current-world (world 0 0))
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
  (current-fragment-id '()))


(serializable-struct state
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
                      ))

(define (save)
  (define output-file (open-output-file "save.txt" #:exists 'truncate)) ; truncate = delete if exists

  (define st (state
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
              (current-fragment-id)))
  (define serialized-state (serialize st))
  (write serialized-state output-file)

  
  #;(write serialized-situation output-file)
  (close-output-port output-file))


; NOTE: "Serialization followed by deserialization produces a value with the same graph structure and mutability as the original value, but the serialized value is a plain tree (i.e., no sharing)."
; - https://docs.racket-lang.org/reference/serialization.html
(define (load-situation-from-state serialized-state)
  (define deserialized-state (deserialize serialized-state))
  
  (current-world (state-world deserialized-state))
  (current-log (state-log deserialized-state))
  (current-last-paragraph (state-last-paragraph deserialized-state))
  (current-part (state-part deserialized-state))
  (current-chapter (state-chapter deserialized-state))
  (current-prompt (state-prompt deserialized-state))
  (current-pending-action (state-pending-action deserialized-state))
  (current-times-begin-traverse-narrated (state-times-begin-traverse-narrated deserialized-state))
  (current-times-finish-traverse-narrated (state-times-finish-traverse-narrated deserialized-state))
  (current-times-cancel-traverse-narrated (state-times-cancel-traverse-narrated deserialized-state))
  (current-times-species-encountered (state-times-species-encountered deserialized-state))
  (current-flags (state-flags deserialized-state))
  (current-round (state-round deserialized-state))
  (current-run (state-run deserialized-state))
  (current-elapsed-time (state-elapsed-time deserialized-state))
  (current-in-combat? (state-in-combat? deserialized-state))
  (current-quests (state-quests deserialized-state))
  (current-persistent-quests (state-persistent-quests deserialized-state))
  (current-pc (state-pc deserialized-state))
  (current-life (state-life deserialized-state))
  (current-fragment-id (state-current-fragment-id deserialized-state))
  
  )