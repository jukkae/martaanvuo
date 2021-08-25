#lang racket

(provide resolve-pc-action!)

(require "../state/logging.rkt"
         "../state/state.rkt")

(require "../action.rkt"
         "../action-resolver.rkt"
         "../io.rkt"
         "../location.rkt"
         "../locations.rkt"
         "../route.rkt"
         "../utils.rkt")

(require "event.rkt"
         "event-handler.rkt"
         "simulation.rkt"
         "timeline.rkt")

; may return:
; void
; 'end-run
; 'win-game
; 'interrupted
; 'success
; 'failure
(define (resolve-pc-action! action)
  (define elapsed-time 0)
  (define result
    (let/ec return

      ; pre-resolve
      (case (action-symbol action)
        
        ['cancel-traverse
         (dev-note "cancel-traverse broken")]

        [else
         (when (and (not (eq? (action-symbol action) 'traverse))
                    (not (eq? (action-symbol action) 'cancel-traverse)))
           ; begin advancing time
           (define timeline
             (advance-time-until-next-interesting-event! (action-duration action)))
           (set! elapsed-time (timeline-duration timeline))

           #;(narrate-timeline timeline)

           ; look into https://docs.racket-lang.org/rebellion/Enum_Types.html for enums etc
           (when (eq? (timeline-metadata timeline) 'interrupted)
             (handle-pc-action-interrupted! timeline)
             (return 'interrupted))
           )])
                   
      ; should check results maybe here?
      (define action-result (resolve-action! action))

      ; post-resolve

      (case (action-symbol action)
        ['go-to-location
         (define next-location (action-target action))
         (move-pc-to-location! next-location)
         (location-on-enter! (current-location))

         (describe-finish-traverse-action action)
                          
         (when (not (null? (location-items (action-target action))))
           (pick-up-items!))]
        
        
        
        ['cancel-traverse
         (reset-pending-action!)
         (move-pc-to-location! (action-target action))

         (describe-cancel-traverse-action action)
         (display-location-info-card (current-location))
         (when (not (null? (location-items (action-target action))))
           (pick-up-items!))])
                   
      action-result
      ))

  ; do the state management mutation stuff
  (when (eq? 'interrupted result) (set-pending-action! action elapsed-time))
  result
  )


(define (set-pending-action! action elapsed-time)
  (define time-left (- (action-duration action) elapsed-time))
  (define pending-action action)
  (set-action-duration! pending-action time-left)
  (set-action-details! pending-action (append-element (action-details pending-action) 'pending))
  (current-pending-action pending-action))

