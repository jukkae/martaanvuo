#lang racket

(provide (all-defined-out))
(provide (all-from-out "fragment-handler.rkt"
                       "ui.rkt"))
(provide print-meta-commands-with-keys
         meta-command-valid?)

(require racket/serialize)

(require lens)

(require "../action-resolver.rkt")
(require "../action.rkt")
(require "../actions.rkt")
(require "../actor.rkt")
(require "../blindscraper.rkt")
(require "../character-sheet.rkt")
(require "../choice.rkt")
(require "../condition.rkt")
(require "../decision.rkt")
(require "../fragment.rkt")
(require "../fragments.rkt")
(require "../grabberkin.rkt")
(require "../io.rkt")
(require "../item.rkt")
(require "../location.rkt")
(require "../locations.rkt")
(require "../pc.rkt")
(require "../quest.rkt")
(require "../round-summary.rkt")
(require "../route.rkt")
(require "../situation.rkt")
(require "../time.rkt")
(require "../utils.rkt")
(require "../world.rkt")

(require "action-queue.rkt"
         "ai.rkt"
         "event.rkt"
         "fragment-handler.rkt"
         "get-next-pc-action.rkt"
         "round.rkt"
         "simulation.rkt"
         "timeline.rkt"
         "ui.rkt")


; engine / round resolver
(define (enqueue-npc-actions)
  (define actors (location-actors (current-location)))
  (for ([actor actors])
    (when (not (pc-actor? actor))
      (define next-action (get-next-action actor))
      (add-to-action-queue next-action))))

; engine / round resolver
(define (resolve-turn! world action)
  (if (pc-actor? (action-actor action))
      (resolve-pc-action! action)
      (resolve-npc-action! action))
  )




; engine / round resolver
; MAIN RESOLVER ENTRYPOINT
(define (resolve-round mode)
  (on-begin-round mode)
  
  (enqueue-npc-actions)
  
  (if (eq? mode 'continue)
      (redescribe-situation)
      (describe-situation))
  
  (save)
  (let/ec end-round-early-with-round-status
    (define pc-action (get-next-pc-action))
    
    (cond ((eq? pc-action 'end-round-early)
           (on-end-round)
           (end-round-early-with-round-status 'ok))
          ((eq? pc-action 'restart)
           (end-round-early-with-round-status 'restart))
          ((eq? pc-action 'recurse)
           (end-round-early-with-round-status 'recurse))
          ((eq? pc-action 'end-chapter)
           (on-end-round)
           (next-chapter!)
           (end-round-early-with-round-status 'ok))
          (else

           (describe-pc-intention pc-action)
  
           

           (define round-exit-status 'ok)
           (cond ((initiative-based-resolution? pc-action)
                  (add-to-action-queue pc-action)
                  (update-npc-reactions pc-action)
                  (sort-action-queue)
                  (resolve-turns!))
                 (else
                  (define pc-action-result (resolve-pc-action! pc-action))
                  (when (eq? 'end-run pc-action-result) (set! round-exit-status 'end-run))
                  (when (eq? 'win-game pc-action-result) (set! round-exit-status 'win-game))))
           (on-end-round)
           (when (not (pc-actor-alive? (pc))) (set! round-exit-status 'pc-dead))
           round-exit-status
           ))))


; may return:
; void
; 'end-run
; 'win-game
; 'interrupted
; 'success
; 'failure
(provide resolve-pc-action!)
(define (resolve-pc-action! action)
  (define elapsed-time 0)
  (define result (let/ec return
                   ; do these BEFORE action resolution

                   ; TODO this is heavy on narration -> is this a fragment?
                   (cond ((eq? (action-symbol action) 'end-run)
                          (cond ((flag-set? 'ending-run-allowed)
                                 #;(p "At least it's something.")
                                 (return 'end-run))
                                (else
                                 (set-flag 'tried-to-go-back)
                                 (p "The unexpected fork is worrisome. Otava must have taken the wrong turn somewhere. She decides to turn back, make sure she hasn't missed anything.")
                                 (wait-for-confirm)
                                 (next-chapter!) ; end chapter
                                 (p "Otava is getting close to what she's looking for, but she has trouble remembering how she got here. Did she follow the trail of the Broker? Yes, yes she did. What was she doing here?")
                                 (wait-for-confirm)
                                 (p "The Facility. She is looking for the Facility at Martaanvuo, to pay back her debt to the Collector. Broker's trail comes to a fork.")
                                 (p "To the left, the trail turns into a climb up a rocky hill. A magpie's call echoes from somewhere up the hill. An army of ants is marching down the other branch, toward what must be Martaanvuo swamp.")
                                 (return 'failure)
                                 )))
                         ((eq? (action-symbol action) 'win-game)
                          (return 'win-game))
                         
                         ((and (eq? (action-symbol action) 'traverse)
                               (not (pending? action)))
                          (describe-begin-traverse-action action)))

                   ; TODO: think about how this actually interacts with elapse-time;
                   ; likely, elapse-time should take a parameter: whether or not to have time-dependent random events
                   (when (and (not (eq? (action-symbol action) 'traverse))
                              (not (eq? (action-symbol action) 'cancel-traverse)))
                     ; begin advancing time
                     (define timeline
                       (advance-time-until-next-interesting-event! (action-duration action)))
                     (set! elapsed-time (timeline-duration timeline))

                     ; display events
                     (define
                       displayable-events
                       (map
                        (λ (event)
                          (list
                           (string-append " " (number->string (event-at event)) " ")
                           (string-append " " (symbol->string (event-type event)) " ")
                           (string-append " " (~s (event-details event)) " ")
                           (string-append " "
                                          (if (event-interrupting? event)
                                              "yes"
                                              "no")
                                          " ")
                           ))
                        (timeline-events timeline)))
                     #;(info-card
                        (append
                         (list (list " at " " type " " details " " interrupts action? "))
                         displayable-events)
                        (string-append "Timeline, duration " (number->string (timeline-duration timeline))))
                     (for ([event (timeline-events timeline)])
                       (narrate-event event))

                     ; look into https://docs.racket-lang.org/rebellion/Enum_Types.html for enums etc
                     (when (eq? (timeline-metadata timeline) 'interrupted)
                       (handle-pc-action-interrupted! timeline)
                       (return 'interrupted))
                     )
    
    
                   ; should check results maybe here?
                   (define action-result (resolve-action! action))
                   
                   ; do these AFTER action resolution
                   (cond ((eq? (action-symbol action) 'go-to-location)
                          (define next-location (action-target action))
                          (move-pc-to-location! next-location)
                          (location-on-enter! (current-location))

                          (describe-finish-traverse-action action)
                          (display-location-info-card (current-location))
                          (when (not (null? (location-items (action-target action))))
                            (pick-up-items!))
                          ))

                   ; TODO duplication, clean up
                   (cond ((eq? (action-symbol action) 'traverse)
                          (move-pc-to-location! (action-target action))
                          (when (not (pending? action))
                            ; -> roll-for-encounter or something, more content than code -> belongs elsewhere

                            (define encounter-roll
                              (if (not (route-has-detail? (current-location) 'no-encounters))
                                  (d 1 6)
                                  #f))

                            (when encounter-roll
                              (displayln "encounter roll: ")
                              (displayln encounter-roll)
                              (define encounter-event
                                (if (< 4 encounter-roll)
                                    (make-event 'spawn-enemies
                                                '() ; pack info about enemies / event here
                                                #t)
                                    '()))

                              (cond ((not (null? encounter-event))
                                     ; create timeline to leverage existing code
                                     (define events
                                       (list
                                        (make-event 'spawn-enemies
                                                    '() ; pack info about enemies / event here
                                                    #t)))
                                     (define metadata 'interrupted)
                                     (define duration 1) ; half of traversal time - where to elapse the rest? after the event, likely
                                     (define tl (timeline metadata events duration))

                                     (set! elapsed-time (timeline-duration tl))

                                     ; DUPLICATION, clean up
                                     ; display events
                                     (define
                                       displayable-events
                                       (map
                                        (λ (event)
                                          (list
                                           (string-append " " (number->string (event-at event)) " ")
                                           (string-append " " (symbol->string (event-type event)) " ")
                                           (string-append " " (~s (event-details event)) " ")
                                           (string-append " "
                                                          (if (event-interrupting? event)
                                                              "yes"
                                                              "no")
                                                          " ")
                                           ))
                                        (timeline-events tl)))
                                     #;(info-card
                                        (append
                                         (list (list " at " " type " " details " " interrupts action? "))
                                         displayable-events)
                                        (string-append "Timeline, duration " (number->string (timeline-duration tl))))
                                     (for ([event (timeline-events tl)])
                                       (narrate-event event))

                                     ; look into https://docs.racket-lang.org/rebellion/Enum_Types.html for enums etc
                                     (when (eq? (timeline-metadata tl) 'interrupted)
                                       (handle-pc-action-interrupted! tl)
                                       (return 'interrupted))))))
                          


                          (set-route-traversed! (action-target action)) ; I think this should work

                          (define next-location (if (memq 'a-to-b (action-details action))
                                                    (route-b (action-target action))
                                                    (route-a (action-target action))))
                          (move-pc-to-location! next-location)


                          (describe-finish-traverse-action action)
                          (display-location-info-card (current-location))
                          (when (not (null? (location-items (action-target action))))
                            (pick-up-items!))
                          ))

                   ; TODO TRIPLICATION CLEAN THIS SHIT UP
                   (cond ((eq? (action-symbol action) 'cancel-traverse)
                          (reset-pending-action!)
                          (move-pc-to-location! (action-target action))

                          (describe-cancel-traverse-action action)
                          (display-location-info-card (current-location))
                          (when (not (null? (location-items (action-target action))))
                            (pick-up-items!))
                          ))
                   
                   action-result
                   ))

  ; do the state management mutation stuff
  (when (eq? 'interrupted result)
    (define time-left (- (action-duration action) elapsed-time))
    (define pending-action action)
    (set-action-duration! pending-action time-left)
    (set-action-details! pending-action (append-element (action-details pending-action) 'pending))
    (set-pending-action! pending-action))
  result
  )

; engine / round resolver
(define (handle-interrupting-event! event)
  (cond ((eq? (event-type event) 'spawn-enemies)
         (spawn-enemies (current-location))
         )
        (else
         (displayln "handle-interrupting-event!: unknown event type")))
  '())

; engine / round resolver
(define (handle-pc-action-interrupted! timeline)
  (define interrupting-events
    (filter
     (λ (event) (event-interrupting? event))
     (timeline-events timeline)))
  
  (cond ((eq? (length interrupting-events) 1)
         (define event (car interrupting-events))
         (handle-interrupting-event! event)
         )
        (else
         (displayln "handle-pc-action-interrupted!: unexpected amount of interrupting events")))
  )

; engine / round resolver
(define (resolve-npc-action! action)
  (resolve-action! action))

(define (end-combat)
  (remove-all-enemies-and-end-combat!)
  (clear-action-queue!))

; engine / round resolver
(define (resolve-turns!)
  (let/ec end-round-early
    (when (all-actions-of-type? action-queue 'flee)
      (p "Otava turns her back to flee and crawls under a bush to hide. She waits a while. Nothing seems to be following her.")
      (award-xp! 1)
      (end-combat)
      (end-round-early))
    (for ([action action-queue])

      (define actor (action-actor action))
      
      (define pre-action-reaction? (get-pre-action-reaction action))
      (when (not (null? pre-action-reaction?))
        (set! action pre-action-reaction?))
      
      (define turn-result (resolve-turn! world action))

      ; todo
      (define post-action-reaction-from-target? (get-post-action-reaction action turn-result))
      (when (not (null? post-action-reaction-from-target?))
        ;(define action post-action-reaction-from-target?)
        (displayln "-- post-action-reaction-from-target?: handle!"))
      
      (case turn-result
        
        ['pc-dead
         (end-round-early)]
        
        ['end-combat
         (end-combat)
         (end-round-early)
         ]

        ; TODO: As always, description belongs in the action
        ['grip-released
         (p "The Grabberkin's hands let go of Otava's ankles and disappear under the moss.")
         (award-xp! 3 "for surviving an encounter with a Grabberkin")
         (remove-enemy actor)
         ]
        )
      )
    ))



(define (save)
  (save-situation *situation*))
