#lang racket

(require racket/generator)
(require racket/serialize)

(require lens)
(require text-table)

(require "action-resolver.rkt")
(require "action.rkt")
(require "actions.rkt")
(require "actor.rkt")
(require "blindscraper.rkt")
(require "checks.rkt")
(require "character-sheet.rkt")
(require "fragment.rkt")
(require "fragments.rkt")
(require "grabberkin.rkt")
(require "info-card.rkt")
(require "location.rkt")
(require "pc.rkt")
(require "situation.rkt")
(require "utils.rkt")
(require "world.rkt")


(provide award-xp!)
(define (award-xp! amount . reason)
  (if (null? reason)
      (displayln (string-append "[+" (number->string amount) " xp]"))
      (displayln (string-append "[+" (number->string amount) " xp " (car reason) "]")))
  (define pc (situation-pc *situation*))
  (set-pc-actor-xp! pc
                    (+ (pc-actor-xp pc)
                       amount)))


(define (current-fragment-on-begin-round!)
  (paragraph (story-fragment-description (situation-current-fragment *situation*)))
  )


(define (current-fragment-get-decisions)
  (filter (lambda (potential-decision)
            ((decision-requirement potential-decision)))
          (story-fragment-decisions (situation-current-fragment *situation*))))

(define (current-fragment-handle-decision! decision)

  (paragraph (decision-description decision))
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
(define (current-scene-on-end-round!)
  '()
  )

(define (go-to-story-fragment id)
  (set-situation-current-fragment! *situation* (get-fragment id))
  ((story-fragment-on-enter! (situation-current-fragment *situation*))))


(provide pc)
(define (pc)
  (situation-pc *situation*))

(provide in-combat?)
(define (in-combat?)
  (situation-in-combat? *situation*))

(provide set-in-combat?!)
(define (set-in-combat?!)
  (set-situation-in-combat?! *situation*))

(define (remove-all-enemies-and-end-combat!)
  (for ([enemy (get-current-enemies)])
    (hash-remove! *enemy-stances* enemy)
    (remove-actor-from-location! (actor-current-location enemy) enemy))
  (set-situation-in-combat?! *situation* #f))

(serializable-struct event
                     (type
                      details
                      interrupting?
                      at)
                     #:constructor-name event*)

(define (make-event
         type
         details
         interrupting?)
  (event* type details interrupting? (world-elapsed-time (situation-world *situation*))))
   

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
       (define dice-sides 100) ; tweak on a per-location basis
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



(define (inventory)
  (define actor (situation-pc *situation*))
  
  (define sheet
    (append
     (list
      (list " Item " " Notes "))
     (actor-inventory actor)))
  (info-card
   sheet
   "Inventory"
   )
  #t
  )

; TODO move to state
; currently: quest - status - notes
; and table-display formatted
(define *quests* '())

(provide create-quest)
(define (create-quest quest-symbol)
  (define quest
    (case quest-symbol
      ['pay-off-debt
       (list " pay off the debt to the Collector "
             " in progress "
             " unsettled: 4,328 grams of U-235 ")]
      ['the-anthead
       (list " seek the Anthead Girl "
             " not started "
             " \"not ready yet\" ")]))
  (set! *quests*
        (append-element *quests* quest))

  (info-card
   (list quest)
   "New quest")
  )

(define (quests)
  (define sheet
    (append
     (list
      (list " quest " " status " " notes ")
      )
     *quests*
     ))
  (info-card
   sheet
   "Quests")
  )

(provide current-location)
(define (current-location)
  #;(displayln "-- current-location: TODO move to situation")
  (actor-current-location (situation-pc *situation*)))

(provide clean-up-dead-actor!)
(define (clean-up-dead-actor! actor)
  (hash-remove! *enemy-stances* actor)
  (set-location-actors! (current-location) (remove actor (location-actors (current-location))))
  (define corpse (cons 'corpse "Blindscraper corpse"))
  (displayln "clean-up-dead-actor!: todo: add corpse")
  (displayln corpse))

(provide actor-in-range?)
(define (actor-in-range? enemy range)
  (define stance (hash-ref *enemy-stances* enemy))
  (eq? (stance-range stance) range))
         

(define (get-next-npc-action actor)
  (case (actor-name actor)
    (["Blindscraper"] (get-blindscraper-action actor))
    (["Grabberkin"] (get-grabberkin-action actor))
    (else (displayln "get-next-npc-action: unknown actor"))))


(define (get-next-action actor)
  (cond ((not (pc-actor? actor)) (get-next-npc-action actor))
        (else
         (serialize-state)
         (get-next-pc-action)))
  )

(define (remove-actor-from-its-current-location! actor)
  (define current-location (actor-current-location actor))
  (when (not (eq? '() current-location))
    (remove-actor-from-location! current-location actor)))

(provide move-actor-to-location!)
(define (move-actor-to-location! actor location)
  (remove-actor-from-its-current-location! actor)
  (set-actor-current-location! actor location)
  (add-actor-to-location! location actor))



(define action-queue '())
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

(define (add-to-action-queue action)
  (set! action-queue (cons action action-queue)))
(define (remove-from-action-queue actions)
  (set! action-queue (remq* actions action-queue)))

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
  
  (info-card actions "Action initiatives")

  (set! action-queue '())
  (for ([action-with-initiative sorted])
    (set! action-queue (append-element action-queue (cdr action-with-initiative))))
  
  action-queue)

(define (enqueue-npc-actions)
  (define actors (location-actors (current-location)))
  (for ([actor actors])
    (when (not (pc-actor? actor))
      (define next-action (get-next-action actor))
      (add-to-action-queue next-action))))


; TODO this belongs to AI
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

; These should be stored in the action
(define (describe-pc-intention pc-action)
  (case (action-symbol pc-action)
    ['forage (paragraph "Otava is getting low on supplies. Too low to be comfortable. Here looks good as any, so she decides to take a look around, see if there's anything edible.")]
    #;[else (paragraph "TBD")]))
(define (describe-begin-go-to-action action)
  (cond ((eq? 'ruins (location-type (action-target action)))
         "The hillside is steep and slippery.")
        ((eq? 'swamp (location-type (action-target action)))
         "The path soon disappears entirely, and a dense, suffocating fog obscures what little visibility there is through the bushes and thickets of small trees. Here and there are palm-sized patches of asphalt sticking through, fighting overgrown mosses.")
        ("[[begin-go-to description not written yet]")))
(define (describe-finish-go-to-action action)
  (cond ((eq? 'ruins (location-type (action-target action)))
         "Eventually, Otava gets to the top.")
        ((eq? 'swamp (location-type (action-target action)))
         "After a while, Otava finds herself in the middle of the swamps. Through the heavy fog, the bushes swaying in the wind look like evil beast-shadows.")
        ("[[finish-go-to description not written yet]")))

(define (meta-command-valid? meta-commands-with-keys input)
  (set! input (string-upcase input))
  (define meta-command (hash-ref meta-commands-with-keys input '()))
  (if (not (null? meta-command))
      meta-command
      #f))

(define (choice-valid? choices-with-keys input)
  (define choice (hash-ref choices-with-keys (string->number input) '()))
  (if (not (null? choice))
      choice
      #f))

(define (scene-decision-valid? decisions-with-keys input)
  (define decision (hash-ref decisions-with-keys (string->number input) '()))
  (if (not (null? decision))
      decision
      #f))

(define (choice-as-action choices-with-keys input)
  ((choice-resolution-effect (hash-ref choices-with-keys (string->number input) '()))))

(define (handle-fragment-decision decisions-with-keys input)
  (define decision (hash-ref decisions-with-keys (string->number input)))
  (current-fragment-handle-decision! decision))

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


      (define scene-decisions (if (null? (situation-current-fragment *situation*))
                                  '()
                                  (current-fragment-get-decisions)))
      (define world-choices (get-world-choices (situation-world *situation*) actor))
      
      (define choices (if (null? scene-decisions)
                          world-choices
                          '()))

      (define scene-decisions-with-keys (build-keys-to-choices-map scene-decisions 1))
      (define first-non-scene-index (add1 (length scene-decisions)))
      (define choices-with-keys (build-keys-to-choices-map choices first-non-scene-index)) ; should check for pending actions and name choices accordingly
      (define meta-commands-with-keys (get-meta-commands-with-keys))
      
      (print-choices-and-meta-commands-with-keys choices-with-keys scene-decisions-with-keys meta-commands-with-keys verbosity)
      (define input (wait-for-input))
      (serialize-input)

      (newline)

      (cond ((meta-command-valid? meta-commands-with-keys input) (handle-meta-command meta-commands-with-keys input))
            ((scene-decision-valid? scene-decisions-with-keys input)
             (begin
               (handle-fragment-decision scene-decisions-with-keys input)
               produce-action 'end-round-early))
            ((choice-valid? choices-with-keys input) (produce-action (choice-as-action choices-with-keys input)))
            (else (what-do-you-do 'abbreviated))))))

(provide paragraph)
(define (paragraph . args)
  (displayln (string-append* args))
  (newline))

(define (title)
  (newline)
  (displayln "M A R T A A N V U O")
  (displayln "===================")
  (newline))

(define (print-choices-with-keys choices-with-keys)
  ; TODO: Should order here based on key
  (for ([(k v) (in-hash choices-with-keys)])
    (displayln (string-append "[" (number->string k) "]: " (choice-name v))))
  (newline))

(define (print-decisions-with-keys decisions-with-keys)
  (for ([(k v) (in-hash decisions-with-keys)])
    (displayln (string-append "[" (number->string k) "]: " (decision-title v))))
  #;(newline))
  

(define (key-from-index i)
  (cond ((< i 0) (error "negative index!"))
        ((<= i 8) (add1 i))
        ((= i 9) 0)
        ((> i 9) (error "too many things to do!"))))

(define (build-keys-to-choices-map choices first-index)
  (define choices-with-keys (make-hash))
  (for ([i (in-range (length choices))])
    (define key (key-from-index (+ first-index i -1)))
    (hash-set! choices-with-keys key (list-ref choices i)))
  choices-with-keys)

(provide wait-for-confirm)
(define (wait-for-confirm)
  (displayln "[Enter]")
  (newline)
  (define input (read-line))
  input)

(define (actor-status-card actor title)
  (info-card
   (list
    (list
     (string-append " " (actor-name actor) " ")
     "")
    (list
     " hp: "
     (string-append
      " "
      (number->string (actor-hp actor))
      "/"
      (number->string (actor-max-hp actor))
      " ")))
   title))


(define (handle-exploration-check-result! result)
  (if result
      (begin
        (expose-neighbor! (current-location))
        'successful)
      (begin
        (displayln "Exploration failed.")
        'failure)))

(define (resolve-pull-under-action! action)
  (define roll "2d6+whatever")
  (displayln "The gk tries to pull Otava under")
  (displayln roll)
  'ok
  )

(define (resolve-anklebreaker-action! action)
  (define roll "2d6+whatever")
  (displayln "Anklebreaker resolution")
  (displayln roll)
  'ok
  )

(define (resolve-tighten-grip-action! action)
  (define roll "2d6+whatever")
  (displayln "The gk tightens its grip")
  (displayln roll)
  'ok
  )

; can return:
; 'pc-dead  when the pc is dead as a consequence of this action
; 'ok       when the action is completely resolved and not explicitly successful or failed
; 'success  when the action is completely resolved and explicitly successful
; 'failed   when the action is completely resolved and fails
(define (resolve-action! action)
  (when (actor-alive? (action-actor action))
    (cond ((eq? (action-symbol action) 'melee)
           (define result (resolve-melee-action! action))
           (when (eq? result 'dead)
             (if (not (pc-actor? (action-target action)))
                 (paragraph "The " (actor-name (action-target action)) " is dead.")
                 (begin
                   (paragraph "Otava is dead.")
                   'pc-dead))))

          ((eq? (action-symbol action) 'wrestle)
           (define result (resolve-wrestle-action! action))
           (when (eq? result 'dead)
             (if (not (pc-actor? (action-target action)))
                 (paragraph "The " (actor-name (action-target action)) " is dead.")
                 (begin
                   (paragraph "Otava is dead.")
                   'pc-dead))))

          ((eq? (action-symbol action) 'forage)
           (begin
             (define skill 0)
             (define target 8)
             
             (define successful? (skill-check "Forage" skill target))
             
             
             (cond (successful?
                    (define amount (d 1 4)) ; portions = days of survival
                    (define amount-string
                      (if (= amount 1)
                          (string-append (number->string amount) " meal")
                          (string-append (number->string amount) " meals")))

                    (info-card
                     (list
                      (list
                       " 1d4 "
                       " = "
                       (string-append " " amount-string " "))
                      )
                     "Forage results roll")
                    (paragraph "After some time, Otava finds some edible fruits and roots. (" (number->string amount) " meals.)")
                    (define item (list 'food (list amount)))
                    (add-item-to-inventory! (situation-pc *situation*) item)
                    )
                   (else
                    (begin
                      (paragraph "Despite spending a while, Otava can't find anything to eat.")
                      (define luck-roll (d 1 20))
                      (info-card
                       (list
                        (list
                         " 1d20 "
                         " = "
                         (string-append " " (number->string luck-roll) " " )))
                       "Luck roll")
                      )))
             ))
          ((eq? (action-symbol action) 'back-off)
           'ok
           )
          ((eq? (action-symbol action) 'go-to-location)
           'ok
           )
          ((eq? (action-symbol action) 'search-for-paths)
           (define exploration-skill (get-trait (situation-pc *situation*) "exploration-skill"))
           (define target-number 9)
           (define exploration-check-result (skill-check "Exploration" exploration-skill target-number))
           (handle-exploration-check-result! exploration-check-result))
          ((eq? (action-symbol action) 'sleep)
           (paragraph "Otava turns in for the night. Get some rest.")
           'ok)
          ((eq? (action-symbol action) 'flee)
           (cond ((pc-actor? (action-actor action))
                  (paragraph "Otava turns her back to run.")
                  (define skill (get-trait (situation-pc *situation*) "athletics-skill"))

                  (define stance-range-values '())
                  (for ([(k v) (in-hash *enemy-stances*)])
                    (define value (get-stance-range-numeric-value (stance-range v)))
                    (set! stance-range-values (append-element stance-range-values value)))
                  (define target-number
                    ; if there's an enemy in engaged range, then more difficult check
                    (if (member 0 stance-range-values)
                        10
                        8))
           
                  (define success? (skill-check "Athletics" skill target-number))
                  (if success?
                      (begin ; TODO wouldn't it be cool if only failure was explicitly noted :D
                        (paragraph "She dives behind a small bush and waits. Nothing seems to be following her.")
                        (award-xp! 3 "for a working survival instinct")
                        'escape-from-combat)
                      (begin
                        (paragraph "Otava's foot gets caught on a root. She falls face down in the mud.")
                        (actor-add-status! (action-target action) 'fallen 1)
                        (display-pc-combatant-info (situation-pc *situation*))
                        (wait-for-confirm)
                        'failure))
                  )

                 (else ; not a pc actor
                  (paragraph
                   (string-append
                    (get-combatant-name (action-actor action))
                    " tries to run."))
                  (define skill 1)
                  (define stance (hash-ref *enemy-stances* (action-actor action)))
                  (define value (get-stance-range-numeric-value (stance-range stance)))
                  (define target-number
                    (if (= value 0)
                        10
                        8))
           
                  (define success? (skill-check "Athletics" skill target-number))
                  (if success?
                      ; TODO this fails if there are multiple enemies!
                      (begin
                        (paragraph "The Blindscraper skitters away and disappears in the foliage.")
                        (award-xp! 1)
                        'escape-from-combat)
                      (begin
                        (paragraph "It is fast, but not fast enough.")
                        (actor-add-status! (action-actor action) 'fallen 1)
                        (display-combatant-info (action-actor action))
                        'failure))
                  ))
           )

          ; This is starting to get unwieldy... but get poc done first
          ((eq? (action-symbol action) 'go-to-engaged)

           (define lp (pc-actor-lp (situation-pc *situation*)))
           (define dex (actor-dexterity (action-actor action)))
           (define success?
             (cond ((positive? lp)
                    (displayln "[LP positive]")
                    (attribute-check "Dexterity" dex))
                   (else #t)))
           
           (if success?
               (begin
                 (paragraph "The Blindscraper suddenly leaps forward and gets a hold of Otava's forearm with a couple of its lanky fingers. One of its long claws is swinging free, looking for an opening.")
                 (hash-remove! *enemy-stances* (action-actor action))
                 
                 (let ([enemy-stance (stance "α" 'engaged "right")])
                   (hash-set! *enemy-stances* (action-actor action) enemy-stance)))
        
               (begin
                 (paragraph "The Blindscraper leaps at Otava, but she dives under it and stumbles back to her feet.")
                 (displayln "[-1 LP]")
                 (set-pc-actor-lp! (situation-pc *situation*)
                                   (- (pc-actor-lp (situation-pc *situation*))
                                      1))
                 (when (< (pc-actor-lp (situation-pc *situation*)) 0)
                   (set-pc-actor-lp! (situation-pc *situation*)
                                     0))
                 (displayln (pc-actor-lp (situation-pc *situation*)))
                 'failure))
           'ok
           )

          ((eq? (action-symbol action) 'inflict-status)
           (define target (action-target action))
           (match (action-details action)
             ['blind
              (paragraph "The Blindscraper swings its claw through an opening between Otava's arms. The claw tears diagonally across Otava's face, cutting its way through flesh, scraping bone.")
              (define roll (d 1 2))
              (wait-for-confirm)
              (case roll
                [(1)
                 ; -> next generation: scars where there were wounds, then next: tattoos -> with both giving changes to the build - "the ghost that lived through" (it's often possible to name a reason)
                 (paragraph "A searing pain cuts through her left eye. Blood and intraocular fluid gush down her face.")]
                [(2)
                 (paragraph "A searing pain cuts through her eyes as her vision turns to black.")])
              ]
             [(cons 'bound number)
              (paragraph "The Grabberkin tightens its grip around Otava's ankle.")
              (actor-add-status! target 'bound 3)
              ]
             [else (paragraph "todo: unknown status")])
           'ok
           )

          ((eq? (action-symbol action) 'tighten-grip)
           (resolve-tighten-grip-action! action))

          ((eq? (action-symbol action) 'anklebreaker)
           (resolve-anklebreaker-action! action))

          ((eq? (action-symbol action) 'pull-under)
           (resolve-pull-under-action! action))
          
          ((eq? (action-symbol action) 'release-grip)
           'grip-released)

          ((eq? (action-symbol action) 'skip)
           (cond ((member 'silent (action-details action))
                  'ok)
                 (else
                  'ok)))
          
          (else (error (string-append "resolve-action!: unknown action type " (symbol->string (action-symbol action))))))))


(serializable-struct
 timeline
 (metadata
  events
  duration))
 
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

(provide move-pc-to-location!)
(define (move-pc-to-location! location)
  ; TODO: location on-exit / on-enter triggers here
  #;(displayln (string-append "-- move-pc-to-location!: moving to " (~v location)))
  (remove-actor-from-location! (current-location) (situation-pc *situation*))
  (set-actor-current-location! (situation-pc *situation*) location)
  (add-actor-to-location! location (situation-pc *situation*)))

; maybe the event should narrate itself, how about that, hmm?
(define (narrate-event event)
  (case (event-type event)
    ('new-time-of-day
     (case (event-details event)
       ('afternoon (paragraph "It is now afternoon."))
       ('evening (paragraph (string-append
                             "It is evening. "
                             )))
       ('night (paragraph "Night falls. Brutal, pitch-black night."))
       ('morning (paragraph "It is morning."))
       ))
    ; spawn-enemies is complicated to narrate outside of the event itself, so this is faster
    ('spawn-enemies '())
    (else (displayln (string-append "narrate-event: unknown event type "
                                    (symbol->string (event-type event)))))))

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
                          (define inventory
                            (actor-inventory (situation-pc *situation*)))
                          (displayln inventory)
                          (paragraph "Otava decides to head back to the Shack.")
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
                          (paragraph (describe-finish-go-to-action action))))
                   
                   action-result
                   ))

  ; do the state management mutation stuff
  (when (eq? 'interrupted result)
    (define time-left (- (action-duration action) elapsed-time))
    (set-pending-action! (lens-set action-duration-lens action time-left)))
  result
  )


(define (handle-interrupting-event! event)
  (cond ((eq? (event-type event) 'spawn-enemies)
         (define encounter-types '(blindscraper grabberkin))


         ;(define encounter-type 'grabberkin)
         (define encounter-type 'blindscraper)
         (displayln
          (string-append
           "<< setting encounter-type to "
           (symbol->string encounter-type)
           " >>"))

         
         (case encounter-type
           ['grabberkin

            (spawn-grabberkin-encounter!)
            ; TODO this should happen at the end of the encounter for it to make sense narratively
            (set-situation-grabberkin-encounters!
             *situation*
             (add1 (situation-grabberkin-encounters *situation*)))
            (player-info)]
           ['blindscraper
            (spawn-blindscraper-encounter!)
            ]
           )
         )
        (else
         (displayln "handle-interrupting-event!: unknown event type")))
  '())

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

(define (resolve-npc-action! action)
  (resolve-action! action))

(define (all-actions-of-type? actions type)
  (define predicate
    (λ (action) (eq? (action-symbol action) type)))
  (all-fulfill-predicate? actions predicate))

(define (resolve-turns!)
  (let/ec end-round-early
    (when (all-actions-of-type? action-queue 'flee)
      (paragraph "Otava turns her back to flee and crawls under a bush to hide. She waits a while. Nothing seems to be following her.")
      (award-xp! 1)
      (remove-all-enemies-and-end-combat!)
      (end-round-early))
    (for ([action action-queue])
      (define turn-result (resolve-turn! world action))

      (when (eq? turn-result 'pc-dead) (end-round-early))
      (when (or (eq? turn-result 'escape-from-combat)
                (eq? turn-result 'grip-released)) ; TODO this'll blow up, must handle per opponent
        (remove-all-enemies-and-end-combat!)
        (end-round-early))
      )
    ))

(define (resolve-turn! world action)
  (if (pc-actor? (action-actor action))
      (resolve-pc-action! action)
      (resolve-npc-action! action))
  )



(define (on-end-round)
  (displayln "[End round]")
  (define current-enemies (get-current-enemies))

  (when (= (length current-enemies) 0)
    ; would be nicer to only change when it's currently true, but eh
    (set-situation-in-combat?! *situation* #f))
  
  (when (not (null? (situation-current-fragment *situation*)))
    (current-scene-on-end-round!)) ; TODO scene-rounds should maybe not increase round?

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
  
  
  
  

  (newline) ; This is the "extra" newline that separates rounds
  )

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
           (when (not (actor-alive? (situation-pc *situation*))) (set! round-exit-status 'pc-dead))
           round-exit-status
           ))))

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

(define (wait-for-input)
  (newline)
  (define input (read-line))
  (newline)
  input)

(define (get-meta-commands-with-keys)
  (define meta-commands (make-hash))
  #;(hash-set! meta-commands "D" (cons "[D]: Describe situation again." describe-situation))
  (hash-set! meta-commands "M" (cons "[M]: Menu." menu))
  (hash-set! meta-commands "C" (cons "[C]: Character sheet." character-sheet))
  (hash-set! meta-commands "I" (cons "[I]: Inventory." inventory))
  (hash-set! meta-commands "Q" (cons "[Q]: Quests." quests))
  meta-commands)

(define (print-meta-commands-with-keys meta-commands-with-keys)
  (for ([(k v) (in-hash meta-commands-with-keys)])
    (display (car v))
    (display " "))
  (newline)
  (newline))


(define (print-choices-and-meta-commands-with-keys choices-with-keys scene-decisions-with-keys meta-commands-with-keys verbosity)
  (cond ((eq? verbosity 'abbreviated)
         (display "Unknown command. Known commands: ")
         (for ([(k v) (in-hash scene-decisions-with-keys)]) (display k))
         (for ([(k v) (in-hash choices-with-keys)]) (display k))
         (for ([(k v) (in-hash meta-commands-with-keys)]) (display k))
         (newline)
         )
        (else
         (newline) ; This is extra spacing, should pass a param to paragraph
         #;(paragraph "What do you do?")
         (print-decisions-with-keys scene-decisions-with-keys)
         (print-choices-with-keys choices-with-keys)
         (print-meta-commands-with-keys meta-commands-with-keys))))

(define (narrate-begin-run)
  (info-card
   (list
    (list " run " (string-append " " (number->string (situation-run *situation*)) " ")))
   (string-append "Begin run number " (number->string (situation-run *situation*))))
  (case (situation-run *situation*)
    [(1)
     (paragraph "After a couple of days of following an old blacktop road, Otava reaches the end of Edgeflats. Before her lie the vast swamplands surrounding Martaanvuo. The Collector told her of a pre-Rains laboratory, abandoned and forgotten. Rumor has it, there's a small reactor in the sub-basement, and with luck, Otava will find enough U-235 to settle her debt to the Collector.")]
    [(2)
     (paragraph "As the path descends, temperature climbs, and Otava soon finds herself drenched in sweat.")]))


(define (on-begin-run)
  (set-situation-run! *situation* (add1 (situation-run *situation*)))
  (set-situation-round! *situation* 0)
  (move-actor-to-location! (situation-pc *situation*) edgeflats)
  (narrate-begin-run)
  (go-to-story-fragment 1)
  )

(define (resolve-a-run)
  (on-begin-run)
  (let/ec end-run
    (let loop ()
      (define round-exit-status (resolve-round))
      (when (eq? round-exit-status 'pc-dead) (end-run 'pc-dead))
      (when (eq? round-exit-status 'win-game) (end-run 'win-game))
      (when (eq? round-exit-status 'end-run) (end-run 'end-run))
      (loop))
    ))

(define (resolve-a-life)
  (on-begin-life)
  (let/ec end-life
    (let loop ()
      (define run-exit-status (resolve-a-run))
      (when (eq? run-exit-status 'pc-dead) (end-life 'pc-dead))
      (when (eq? run-exit-status 'win-game) (end-life 'win-game))
      (when (eq? run-exit-status 'end-run)
        (displayln "TODO: END RUN")
        (loop)))
    ))

(define (player-info)
  (define player-status
    (list
     (list " life " (string-append " " (number->string (situation-life *situation*)) " "))
     (list " grabberkin encounters " (string-append " " (number->string (situation-grabberkin-encounters *situation*)) " "))
     ))
     
  (info-card player-status (string-append "Player status"))
  )

(define (on-begin-life)
  (set-situation-life! *situation* (add1 (situation-life *situation*)))
  (set-situation-pc! *situation* (make-new-pc))
  (player-info)  
  )

(define (on-begin-playthrough)
  ;(paragraph "[" "Begin a story" "]")
  (setup-world)
  )

(define (begin-game)
  #; (random-seed 13)
  (title)
  (on-begin-playthrough)
  (let/ec win-game
    (let begin-new-life ()
      (define pc-life-end-status (resolve-a-life))
      (when (eq? pc-life-end-status 'pc-dead)

        (let end-of-life-menu ([verbosity 'verbose])
          (define (handle-meta-command meta-commands-with-keys input)
            (set! input (string-upcase input))
            (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
            (define meta-command (cdr meta-command-with-key))
            (meta-command)
            (end-of-life-menu 'verbose))

          (define meta-commands (make-hash))
          (hash-set! meta-commands "Q" (cons "[Q]: Quit." quit))
          (hash-set! meta-commands "R" (cons "[R]: Reincarnate." begin-new-life)) ; TODO: change prompt dynamically based on meta-progress

          (paragraph "Reincarnate?")
          (print-meta-commands-with-keys meta-commands)
          (define input (wait-for-input))
          (serialize-input)

          (newline)

          (cond ((meta-command-valid? meta-commands input) (handle-meta-command meta-commands input))
                (else (end-of-life-menu 'abbreviated)))))
      (when (eq? pc-life-end-status 'win-game) (win-game))))
  (win-game)
  
  )

(define (win-game)
  (paragraph "Otava dives under the waters of Martaanvuo Spring and forever ceases to exist.")
  (wait-for-input)
  (exit))


(begin-game)