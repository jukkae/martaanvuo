#lang racket

(provide (all-defined-out))

(provide (all-from-out "mutators.rkt"
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
         "logging.rkt"
         "mutators.rkt"
         "pending-action.rkt"
         "resolve-counts.rkt")


(serializable-struct
 situation
 ([world #:mutable]
  [current-fragment-id #:mutable]
  )
 #:transparent)

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


;;; Actual state variables
(define *situation* '())

(define (reset-situation!)
  (set! *situation*
        (let ([new-world (world 0 0)]
              )
          (situation new-world
                     '()
                     )))
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
  (current-pc (make-new-pc)))


(define (unset-current-fragment-id!)
  (set-situation-current-fragment-id! *situation* '()))

(define (set-current-fragment-id! id)
  (set-situation-current-fragment-id! *situation* id))


(define (describe-non-combat-situation)
  (cond ((null? (situation-current-fragment-id *situation*))
         (cond ((eq? (location-id (current-location)) 'perimeter)
                (set-prompt! "Either a climb up the rocky slope to the magpie, or follow the ants to the swamp."))
               ((eq? (location-id (current-location)) 'magpie-hill)
                (p "Natural rock stairs lead back to Perimeter. There's a decrepit industrial building further ahead on the plateau in the fog. There's also a small trail that seems to lead down, towards Martaanvuo swamp.")))
         (cond ((location-has-feature? (current-location) 'magpie-effigy)
                (p "\"Chk-chk\", the magpie calls insistently from the foliage of the skeletonlike forest on the plateau."))))))



(define (describe-situation)
  (when (location-has-feature? (current-location) 'locked-door)
    (cond ((and (pc-has-item? 'revolver)
                (pc-has-ammo-left?))
           (p "There's a door that's locked with a heavy padlock."))
          ((and (pc-has-item? 'bolt-cutters))
           (p "There's a door that's locked with a heavy padlock."))
          (else
           (p "There's a door that's locked with a heavy padlock. If only she had bolt cutters, or something."))))
  (cond
    ((current-in-combat?) (describe-combat-situation))
    (else (describe-non-combat-situation)))
  )

(define (redescribe-situation)
  (cond
    ((current-in-combat?) (describe-combat-situation))
    (else (repeat-last-paragraph)))
  )





(define (save)
  (save-situation *situation*))

(serializable-struct state
                     ([situation #:mutable]
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
                      ))

(define (save-situation s)
  
  (define serialized-situation (serialize s))
  #;(write-save-file serialized-situation)

  (define output-file (open-output-file "save.txt" #:exists 'truncate)) ; truncate = delete if exists

  (define st (state
              *situation*
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
              (current-life)))
  (define serialized-state (serialize st))
  (write serialized-state output-file)

  
  #;(write serialized-situation output-file)
  (close-output-port output-file))

; NOTE: "Serialization followed by deserialization produces a value with the same graph structure and mutability as the original value, but the serialized value is a plain tree (i.e., no sharing)."
; - https://docs.racket-lang.org/reference/serialization.html
(define (load-situation situation)
  #;(displayln situation)
  (define deserialized (deserialize situation))
  (set! *situation* deserialized))


(define (load-situation-from-state serialized-state)
  #;(displayln situation)
  (define deserialized-state (deserialize serialized-state))
  (define situation (state-situation deserialized-state))
  
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
  
  (set! *situation* situation))