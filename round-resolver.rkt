#lang racket

(provide (all-defined-out))

(require racket/serialize)

(require lens)

(require "action-resolver.rkt")
(require "action.rkt")
(require "actions.rkt")
(require "actor.rkt")
(require "blindscraper.rkt")
(require "character-sheet.rkt")
(require "choice.rkt")
(require "condition.rkt")
(require "fragment.rkt")
(require "fragments.rkt")
(require "grabberkin.rkt")
(require "io.rkt")
(require "item.rkt")
(require "location.rkt")
(require "pc.rkt")
(require "quest.rkt")
(require "situation.rkt")
(require "utils.rkt")
(require "world.rkt")

; fragment handler
(define (current-fragment-on-begin-round!)
  (paragraph (story-fragment-description (situation-current-fragment *situation*)))
  )

; fragment handler
(define (current-fragment-get-decisions)
  (filter (lambda (potential-decision)
            ((decision-requirement potential-decision)))
          (story-fragment-decisions (situation-current-fragment *situation*))))

; fragment handler
; move specifics from here to the actual fragment
(define (current-fragment-handle-decision! decision)

  (paragraph (decision-description decision))

  (when (not (null? (decision-on-resolve! decision)))
    ((decision-on-resolve! decision)))
  
  (define next-fragment (decision-next-fragment decision))

  ; brilliant idea x dirty hack
  (when (procedure? next-fragment)
    (set! next-fragment (next-fragment)))
  (cond ((number? next-fragment)
         (go-to-story-fragment next-fragment)
         )
        ((eq? 'exit next-fragment)
         (set-situation-current-fragment! *situation* '()))
        ((eq? 'exit-and-set-build-desperate next-fragment)
         (set-build! 'desperate)
         (set-situation-current-fragment! *situation* '()))
        ((eq? 'exit-and-set-build-bruiser next-fragment)
         (set-build! 'bruiser)
         (set-situation-current-fragment! *situation* '()))
        (else (error (string-append "(current-fragment-handle-decision!): next-fragment type not implemented: " (symbol->string next-fragment)))))
  )

; fragment handler
(define (current-fragment-on-end-round!)
  '()
  )

; fragment handler
(define (go-to-story-fragment id)
  (set-situation-current-fragment! *situation* (get-fragment id))
  ((story-fragment-on-enter! (situation-current-fragment *situation*))))

; fragment handler
(define (handle-fragment-decision decisions-with-keys input)
  (define decision (hash-ref decisions-with-keys (string->number input)))
  (current-fragment-handle-decision! decision))

; engine / round resolver: ai dispatching
(define (get-next-npc-action actor)
  (case (actor-name actor)
    (["Blindscraper"] (get-blindscraper-action actor))
    (["Grabberkin"] (get-grabberkin-action actor))
    (else (displayln "get-next-npc-action: unknown actor"))))

; store in the action, handle calling from here
; -> code to action handler?
(define (describe-begin-go-to-action action)
  (cond ((eq? 'ruins (location-type (action-target action)))
         "The hillside is steep and slippery.")
        ((eq? 'swamp (location-type (action-target action)))
         "The path soon disappears entirely, and a dense, suffocating fog obscures what little visibility there is through the bushes and thickets of small trees. Here and there are palm-sized patches of asphalt sticking through, fighting overgrown mosses.")
        ("[[begin-go-to description not written yet]")))

; store in the action, handle calling from here
; -> code to action handler?
(define (describe-finish-go-to-action action)
  (cond ((eq? 'ruins (location-type (action-target action)))
         "Eventually, Otava gets to the top.")
        ((eq? 'swamp (location-type (action-target action)))
         "After a while, Otava finds herself in the middle of the swamps. Through the heavy fog, the bushes swaying in the wind look like evil beast-shadows.")
        ("[[finish-go-to description not written yet]")))



; engine / round resolver: implementation detail
(define action-queue '())
; engine / round resolver
(define (add-to-action-queue action)
  (set! action-queue (cons action action-queue)))
; engine / round resolver
(define (remove-from-action-queue actions)
  (set! action-queue (remq* actions action-queue)))
; engine / round resolver
(define (sort-action-queue)
  
  (define actions-by-initiatives '())
  (for ([action action-queue])
    (define actor (action-actor action))
    (define dexterity-mod (get-attribute-modifier-for (actor-dexterity actor)))
    
    (define action-mod 0)
    
    (cond ((has-tag? action 'fast)
           (set! action-mod 2))
          ((has-tag? action 'slow)
           (set! action-mod -4)))



    (define dice-1 (d 1 6))
    (define dice-2 (d 1 6))

    (define total (+ dice-1 dice-2 action-mod dexterity-mod))


    (set! actions-by-initiatives (append-element actions-by-initiatives (cons total action))))

  (define shuffled (shuffle actions-by-initiatives)) ; shuffle to avoid sort stability
  (define sorted (sort shuffled
                       (λ (a1 a2) (> (car a1) (car a2))))) ; intentionally flipped: Higher is better

  (define actions
    (for/list ([action-with-initiative sorted])
      (define action (cdr action-with-initiative))
      (define initiative (car action-with-initiative))
      (define action-description
        (string-append
         " "
         (actor-name (action-actor action))
         #;": "
         #;(symbol->string (action-symbol action))
         " "))
      (list action-description (string-append " " (number->string initiative) " "))))
  ; TODO: Only show initiatives when more than one combatant do something, and exclude the ones that only "skip"
  (info-card actions "Action initiatives")
  (wait-for-confirm)

  (set! action-queue '())
  (for ([action-with-initiative sorted])
    (set! action-queue (append-element action-queue (cdr action-with-initiative))))
  
  action-queue)


; engine / round resolver
(define (on-begin-round)
  (set-situation-round! *situation* (add1 (situation-round *situation*)))
  (define round-summary
    (list
     (list " round "
           (string-append
            " "
            (number->string (situation-round *situation*))
            " "))
     (list " current location "
           (string-append
            " "
            (symbol->string (location-type (current-location)))
            " "))
     (list " time of day " (string-append " " (symbol->string (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*)))) " "))
     (list " elapsed time (total) " (string-append " " (number->string (world-elapsed-time (situation-world *situation*))) " "))
     ))
  (info-card round-summary (string-append "Begin round " (number->string (situation-round *situation*))))
  
  (set! action-queue '())
  
  (when (not (null? (situation-current-fragment *situation*)))
    (current-fragment-on-begin-round!))
  )

; engine / round resolver / -> ai?
(define (get-next-action actor)
  (cond ((not (pc-actor? actor)) (get-next-npc-action actor))
        (else
         (serialize-state)
         (get-next-pc-action)))
  )

; engine / round resolver / -> ai?
(define (get-pre-action-reaction action)
  (define actor (action-actor action))
  (cond ((not (pc-actor? actor))
         (cond ((equal? (actor-name actor) "Grabberkin")
                (get-grabberkin-reaction actor))
               (else
                (displayln "unknown non-pc-actor type for reaction")
                '())))
        (else
         (serialize-state)
         ; TODO
         ; (displayln "PC REACTION")    
         '())))

(define (get-post-action-reaction action result)
  (define actor (action-target action))
  ; TODO
  ; this is a chance for the target of an already-resolved action to react
  '())


; engine / round resolver
(define (enqueue-npc-actions)
  (define actors (location-actors (current-location)))
  (for ([actor actors])
    (when (not (pc-actor? actor))
      (define next-action (get-next-action actor))
      (add-to-action-queue next-action))))

; engine / round resolver
(define (on-end-round)
  #;(displayln "[End round]")
  (define current-enemies (get-current-enemies))

  (when (and (in-combat?)
             (= (length current-enemies) 0))
    (end-combat!)
    (go-to-story-fragment 100))
  
  (when (not (null? (situation-current-fragment *situation*)))
    (current-fragment-on-end-round!)) ; TODO fragment-rounds should maybe not increase round?

  ; remove statuses
  (for ([enemy (get-current-enemies)])
    (define name (get-combatant-name enemy))
    (when (not (null? (actor-statuses enemy)))
      (displayln (string-append "[" name ": removed statuses:]"))
      (for ([status (actor-statuses enemy)])
        (displayln status))
      (decrement-actor-status-lifetimes! enemy)))

  (for ([enemy (get-current-enemies)])
    (define name (get-combatant-name enemy))
    (when (not (null? (actor-statuses enemy)))
      (define name (get-combatant-name enemy))
      (define description (~s (actor-statuses enemy)))
    
      (define description-prefix
        (string-append "[" name ": removed statuses: "))
      (define description-suffix "]")
      (decrement-actor-status-lifetimes! enemy)))

  ; urgh
  (when (not (null? (actor-statuses (situation-pc *situation*))))
    (define name (get-combatant-name (situation-pc *situation*)))
    (define description (~s (actor-statuses (situation-pc *situation*))))
    
    (define description-prefix
      (string-append "[" name ": removed statuses: "))
    (define description-suffix "]")
    (decrement-actor-status-lifetimes! (situation-pc *situation*)))

  
  ; proc conditions - TODO this is currently only for PC, fix if needed!
  (define pc-conditions (actor-conditions (pc)))
  (for ([condition pc-conditions])
    ((condition-on-end-round! condition))
    )
  
  
  #;(newline) ; This is the "extra" newline that separates rounds
  #;(wait-for-confirm)
  )

; engine / round resolver
(define (resolve-turn! world action)
  (if (pc-actor? (action-actor action))
      (resolve-pc-action! action)
      (resolve-npc-action! action))
  )

; type used in engine / round-resolver
(serializable-struct
 event
 (type
  details
  interrupting?
  at)
 #:constructor-name event*)

; type used in engine / round-resolver
(define (make-event
         type
         details
         interrupting?)
  (event* type details interrupting? (world-elapsed-time (situation-world *situation*))))

; narration content to event,
; function to call narration in engine / round-resolver
(define (narrate-event event)
  (case (event-type event)
    ('new-time-of-day
     (case (event-details event)
       ('afternoon (notice "It is now afternoon."))
       ('evening (notice "It is now evening."))
       ('night (notice "It is now night."))
       ('morning (notice "It is now morning."))
       ))
    ; spawn-enemies is complicated to narrate outside of the event itself, so this is faster
    ('spawn-enemies '())
    (else (displayln (string-append "narrate-event: unknown event type "
                                    (symbol->string (event-type event)))))))

; engine / round resolver
; timeline of interesting events
(serializable-struct
 timeline
 (metadata
  events
  duration))



; engine / round resolver
; MAIN RESOLVER ENTRYPOINT
(define (resolve-round)
  (on-begin-round)
  (enqueue-npc-actions)
  (describe-situation)
  
  (serialize-state)
  (let/ec end-round-early-with-round-status
    (define pc-action (get-next-pc-action))
    
    (cond ((eq? pc-action 'end-round-early)
           (on-end-round) ; TODO move on-end-round to the escape continuation where it belongs!
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


; engine / round-resolver at first; some of the stuff should go to action definitions etc

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
                   (cond ((eq? (action-symbol action) 'end-run)
                          #;(define inventory
                            (actor-inventory (situation-pc *situation*)))
                          #;(displayln inventory)
                          
                          (paragraph "From the Edgeflats, it's just following the blacktop, until Otava finally arrives at the Shack.")
                          (return 'end-run))
                         ((eq? (action-symbol action) 'win-game)
                          (return 'win-game))
                         ((eq? (action-symbol action) 'go-to-location)
                          (paragraph (describe-begin-go-to-action action))))
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
    
    
                   ; should check results maybe here?
                   (define action-result (resolve-action! action))
                   
                   ; do these AFTER action resolution
                   (cond ((eq? (action-symbol action) 'go-to-location)
                          (define next-location (action-target action))
                          (move-pc-to-location! next-location)
                          (when (eq? (location-type (current-location)) 'crematory)
                            (go-to-story-fragment 11))
                          (when (eq? (location-type (current-location)) 'swamp)
                            (go-to-story-fragment 20))
                          #;(when (eq? (location-type (current-location)) 'workshop)
                              (go-to-story-fragment 200))
                          (when (eq? (location-type (current-location)) 'workshop)
                            (go-to-story-fragment 300))
                          (paragraph (describe-finish-go-to-action action))
                          (display-location-info-card (current-location))
                          (when (not (null? (location-items (action-target action))))
                            (pick-up-items!))
                          ))
                   
                   action-result
                   ))

  ; do the state management mutation stuff
  (when (eq? 'interrupted result)
    (define time-left (- (action-duration action) elapsed-time))
    (set-pending-action! (lens-set action-duration-lens action time-left)))
  result
  )

; engine / round resolver
(define (handle-interrupting-event! event)
  (cond ((eq? (event-type event) 'spawn-enemies)
         (define encounter-types '(blindscraper grabberkin))

         (define encounter-type (take-random (cond ((eq? (location-type (current-location)) 'ridges)
                                                    'blindscraper)
                                                   ((eq? (location-type (current-location)) 'valleys)
                                                    'grabberkin)
                                                   (else (take-random encounter-types)))))

         (case encounter-type
           ['grabberkin

            (spawn-grabberkin-encounter!)
            ; TODO this should happen at the end of the encounter for it to make sense narratively -> basically, combat timeline handling
            (set-situation-grabberkin-encounters!
             *situation*
             (add1 (situation-grabberkin-encounters *situation*)))
            #;(player-info)]
           ['blindscraper
            (spawn-blindscraper-encounter!)
            ]
           )
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

; engine / round resolver
(define (resolve-turns!)
  (let/ec end-round-early
    (when (all-actions-of-type? action-queue 'flee)
      (paragraph "Otava turns her back to flee and crawls under a bush to hide. She waits a while. Nothing seems to be following her.")
      (award-xp! 1)
      (remove-all-enemies-and-end-combat!)
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
         (remove-all-enemies-and-end-combat!)
         (end-round-early)
         ]

        ; TODO: As always, description belongs in the action
        ['grip-released
         (paragraph "The Grabberkin's hands let go of Otava's ankles and disappear under the moss.")
         (award-xp! 3 "for surviving an encounter with a Grabberkin")
         (remove-enemy actor)
         ]
        )
      )
    ))
   

; engine / low-level "world-as-simulation" sub-resolver
; or perhaps world?
; or perhaps just round-resolver?

; increment world time
; return a list of events that occur at new timestamp
(define (advance-time-by-a-jiffy!)
  (define events '())
  (define new-elapsed-time (add1 (world-elapsed-time (situation-world *situation*))))
  (set-world-elapsed-time!
   (situation-world *situation*)
   new-elapsed-time)

  (when (= (modulo (world-elapsed-time (situation-world *situation*)) 100) 0)
    (define suspend-action?
      (eq? (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*)))
           'night))
    (define ev (make-event 'new-time-of-day (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*))) suspend-action?))
    (set! events (append-element events ev)))


  (when (not (in-combat?))
    (cond
      ;; Currently, only spawn enemies at daytime
      ((not (eq? (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*)))
                 'night))
       (define dice-sides 300) ; tweak on a per-location basis
       (define roll (d 1 dice-sides))

       (cond ((= roll 1)
              (define title "Luck roll failure")
              (info-card
               (list (list
                      (string-append " at world time " (number->string (world-elapsed-time (situation-world *situation*))) " ")
                      (string-append " 1d" (number->string dice-sides) " = 1 ")
                      " failure: hostile encounter, spawning enemies "))
               title)
              (define ev
                (make-event 'spawn-enemies
                            '() ; pack info about enemies / event here
                            #t))
              (set! events (append-element events ev))
              (wait-for-confirm)))
       )))
  events
  )


; engine / round resolver
; breaks on first action-suspending event
; and finishes after duration of jiffies,
; returns a timeline of events that occurred with metadata
(define (advance-time-until-next-interesting-event! jiffies)
  (define metadata '())
  (define events '())
  (define counter 0)
  (let/ec break
    (for ([t jiffies])
      (set! counter (add1 counter))
      (define possible-events-at-t (advance-time-by-a-jiffy!))
      (define events-at-t possible-events-at-t) ; they are real events
      (set! events (append events events-at-t))
        
      ; If any of the events suspends action, then return early
      (define contains-action-suspending-event?
        (memf (λ (event) (event-interrupting? event)) possible-events-at-t))

      ; early-exit
      (when contains-action-suspending-event?
        (set! metadata 'interrupted)
        (break))
      ))
  (timeline metadata events counter))












; engine / get-next-pc-action
(define (get-next-pc-action)
  (serialize-state)
  (let/ec produce-action
    (let what-do-you-do ([verbosity 'verbose])
      (define (handle-meta-command meta-commands-with-keys input)
        (set! input (string-upcase input))
        (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
        (define meta-command (cdr meta-command-with-key))
        (meta-command)
        (redescribe-situation)
        (what-do-you-do 'verbose))
      
      (define actor (situation-pc *situation*))


      (define fragment-decisions (if (null? (situation-current-fragment *situation*))
                                     '()
                                     (current-fragment-get-decisions)))
      (define world-choices (get-world-choices (situation-world *situation*) actor))
      
      (define choices (if (null? fragment-decisions)
                          world-choices
                          '()))

      (define fragment-decisions-with-keys (build-keys-to-choices-map fragment-decisions 1))
      (define first-non-fragment-index (add1 (length fragment-decisions)))
      (define choices-with-keys (build-keys-to-choices-map choices first-non-fragment-index)) ; should check for pending actions and name choices accordingly
      (define meta-commands-with-keys (get-meta-commands-with-keys))
      
      (print-choices-and-meta-commands-with-keys choices-with-keys fragment-decisions-with-keys meta-commands-with-keys verbosity)
      (define input (wait-for-input))
      (serialize-input)

      (newline)

      (cond ((meta-command-valid? meta-commands-with-keys input) (handle-meta-command meta-commands-with-keys input))
            ((fragment-decision-valid? fragment-decisions-with-keys input)
             (begin
               (handle-fragment-decision fragment-decisions-with-keys input)
               
               produce-action 'end-round-early))
            ((choice-valid? choices-with-keys input) (produce-action (resolve-choice-and-produce-action! choices-with-keys input)))
            (else (what-do-you-do 'abbreviated))))))


; engine / get-next-pc-action
(define (meta-command-valid? meta-commands-with-keys input)
  (set! input (string-upcase input))
  (define meta-command (hash-ref meta-commands-with-keys input '()))
  (if (not (null? meta-command))
      meta-command
      #f))

; engine / get-next-pc-action
(define (choice-valid? choices-with-keys input)
  (define choice (hash-ref choices-with-keys (string->number input) '()))
  (if (not (null? choice))
      choice
      #f))

; engine / get-next-pc-action
(define (fragment-decision-valid? decisions-with-keys input)
  (define decision (hash-ref decisions-with-keys (string->number input) '()))
  (if (not (null? decision))
      decision
      #f))

; engine / get-next-pc-action
(define (resolve-choice-and-produce-action! choices-with-keys input)
  (define resolution-effect (choice-as-resolution-effect choices-with-keys input))

  (define action
    (cond ((procedure? resolution-effect) (resolution-effect))
          ((action? resolution-effect) resolution-effect)
          (else (error "resolve-choice-and-produce-action!: unknown type"))))

  ; dirty to do this here like this but eh
  (define pending-choice-available? #f)
  (for/hash ([(k v) (in-hash choices-with-keys)])
    (values k
            (begin
              (when (string-prefix? (choice-name v) "[continue]")
                (set! pending-choice-available? #t)))))
  
  ; choice either is pending (= resolve it) or is not, in which case discard pending action
  (when pending-choice-available? (reset-pending-action!))
  
  action)

; engine / get-next-pc-action
(define (choice-as-resolution-effect choices-with-keys input)
  (choice-resolution-effect (hash-ref choices-with-keys (string->number input) '())))

; engine / get-next-pc-action
(define (print-choices-with-keys choices-with-keys)
  ; TODO: Should order here based on key
  (for ([(k v) (in-hash choices-with-keys)])
    (displayln (string-append "[" (number->string k) "]: " (choice-name v))))
  (newline))

; engine / get-next-pc-action
(define (print-decisions-with-keys decisions-with-keys)
  (for ([(k v) (in-hash decisions-with-keys)])
    (displayln (string-append "[" (number->string k) "]: " (decision-title v))))
  #;(newline))
  
; engine / get-next-pc-action
(define (key-from-index i)
  (cond ((< i 0) (error "negative index!"))
        ((<= i 8) (add1 i))
        ((= i 9) 0)
        ((> i 9) (error "too many things to do!"))))

; engine / get-next-pc-action
(define (build-keys-to-choices-map choices first-index)
  (define choices-with-keys (make-hash))
  (for ([i (in-range (length choices))])
    (define key (key-from-index (+ first-index i -1)))
    (hash-set! choices-with-keys key (list-ref choices i)))
  choices-with-keys)

; engine / get-next-pc-action
(define (get-meta-commands-with-keys)
  (define meta-commands (make-hash))
  #;(hash-set! meta-commands "D" (cons "[D]: Describe situation again." describe-situation))
  (hash-set! meta-commands "M" (cons "[M]: Menu." menu))
  (hash-set! meta-commands "C" (cons "[C]: Character sheet." character-sheet))
  (when (not (null? (actor-inventory (pc))))
    (hash-set! meta-commands "I" (cons "[I]: Inventory." inventory)))
  (hash-set! meta-commands "L" (cons "[L]: Logs." display-log))
  (hash-set! meta-commands "Q" (cons "[Q]: Quests." display-quests))
  meta-commands)

; engine / get-next-pc-action
(define (print-meta-commands-with-keys meta-commands-with-keys)
  (for ([(k v) (in-hash meta-commands-with-keys)])
    (display (car v))
    (display " "))
  (newline)
  (newline))

; engine / get-next-pc-action
(define (print-choices-and-meta-commands-with-keys choices-with-keys fragment-decisions-with-keys meta-commands-with-keys verbosity)
  (cond ((eq? verbosity 'abbreviated)
         (display "Unknown command. Known commands: ")
         (for ([(k v) (in-hash fragment-decisions-with-keys)]) (display k))
         (for ([(k v) (in-hash choices-with-keys)]) (display k))
         (for ([(k v) (in-hash meta-commands-with-keys)]) (display k))
         (newline)
         )
        (else
         (newline) ; This is extra spacing, should pass a param to paragraph
         #;(paragraph "What do you do?")
         (print-decisions-with-keys fragment-decisions-with-keys)
         (print-choices-with-keys choices-with-keys)
         (print-meta-commands-with-keys meta-commands-with-keys))))





; UI? meta? scripting api? return value tied to round resolution
(define (quit)
  (displayln "Really quit? [Q] to quit, anything else to continue.")
  (define input (wait-for-input))
  (set! input (string-upcase input))
  (cond ((equal? input "Q")
         (paragraph "Game exited.")
         (exit))
        (else
         (newline)
         #t))) ; mark input as handled

; UI? meta? scripting api? return value tied to round resolution
(define (menu)
  (define (handle-meta-command meta-commands-with-keys input)
    (set! input (string-upcase input))
    (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
    (define meta-command (cdr meta-command-with-key))
    (meta-command))
  (define (close-menu) #t) ; hacky but eh
  
  (displayln "[Menu]")
  (define meta-commands (make-hash))
  (hash-set! meta-commands "Q" (cons "[Q]: Quit Martaanvuo." quit))
  (hash-set! meta-commands "C" (cons "[C]: Close menu." close-menu))

  (for ([(k v) (in-hash meta-commands)])
    (display (car v))
    (display " "))
  (newline)
  (newline)
  (define input (wait-for-input))
  (serialize-input)

  (newline)

  (cond ((meta-command-valid? meta-commands input) (handle-meta-command meta-commands input))
        (else (menu)))
  #t)

; pc? meta? api?
(define (inventory)
  (define actor (situation-pc *situation*))
  
  (define header
    (list
     (list " Item " " Notes ")))

  (define items (actor-inventory actor))
  (define items-list
    (for/list ([item items])
      (cond ((item? item)
             (list
              (string-append " " (item-name item) " ")
              (string-append " " (~v (item-details item)) " ")))
            (else (list
                   (string-append " " (symbol->string item) " ")
                   (string-append " " " " " "))))
      ))
  
  (define sheet
    (append
     header
     items-list))
  
  (info-card
   sheet
   "Inventory"
   )
  #t
  )

; pc? meta?
(define (display-quests)
  (define body
    (for/list ([q (quests)])
      (format-quest-for-card q)))
  (define sheet
    (append
     (list
      (list " quest " " status " " notes ")
      )
     body
     ))
  (info-card
   sheet
   "Quests")
  )



; some sort of generic ai module?
(define (update-npc-reactions pc-action)
  (define npcs (get-current-enemies))
  (when (and (aggressive? pc-action)
             (not (in-combat?)))
    ; remove own actions from queue
    (for ([actor npcs])
      (define actions (filter
                       (λ (action) (eq? actor (action-actor action)))
                       action-queue))
      (remove-from-action-queue actions)
      ; blam blam
      #;(define action (make-shoot-action actor))
      (define action '())
      (add-to-action-queue action))
    )
  )