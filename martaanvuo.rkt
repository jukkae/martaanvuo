#lang racket

(require racket/generator)
(require racket/serialize)

(require lens)
(require text-table)

(require "action.rkt")
(require "actor.rkt")
(require "location.rkt")
(require "utils.rkt")

(serializable-struct
 world
 (locations
  day
  [elapsed-time #:mutable]))

(define edges
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'edges))

(define swamp
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '(search-for-paths)
   #:type 'swamp))

(define ridges
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '(search-for-paths)
   #:type 'ridges))

(define valleys
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '(search-for-paths)
   #:type 'valleys))

(define crematory
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'crematory))

(define ruins
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'ruins))

(define sewers
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'sewers))

(define cache
  (make-location
   #:actors '()
   #:features '()
   #:items '(u-235 veilbreaker-staff)
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'cache))

(define workshop
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '()
   #:type 'workshop))

(define spring
  (make-location
   #:actors '()
   #:features '()
   #:items '()
   #:neighbors '()
   #:tags '()
   #:actions-provided '(dive-in-spring)
   #:type 'spring))

(define (setup-world)
  (set-location-neighbors! edges (list swamp))
  (set-location-neighbors! swamp (list edges ridges valleys))
  (set-location-neighbors! ridges (list swamp))
  (set-location-neighbors! valleys (list swamp))
  (set-location-neighbors! crematory (list valleys))
  (set-location-neighbors! ruins (list ridges sewers cache))
  (set-location-neighbors! sewers (list ruins workshop))
  (set-location-neighbors! cache (list ruins))
  (set-location-neighbors! workshop (list sewers spring))
  (set-location-neighbors! spring (list workshop))
  )

(define (expose-neighbor! location)
  (displayln "exposing neighbor")
  (cond ((eq? (location-type location) 'ridges)
         (displayln "location type ridges")
         (set-location-neighbors! ridges (list swamp ruins))
         )
        (else (error "unknown location type")))
  )

(define (get-attribute-modifier-for attribute)
  (cond ((= attribute 3) -3)
        ((<= 4  attribute  5) -2)
        ((<= 6  attribute  8) -1)
        ((<= 9  attribute 12)  0)
        ((<= 13 attribute 15)  1)
        ((<= 16 attribute 17)  2)
        ((= attribute 18) 3)))

(define (make-new-pc)
  (make-pc-actor
   "Otava"
   3
   4
   ))

(define (set-build! build)
  ; for desperate build, also set a time limit (or whatever other complication)

  (define starting-inventory
    (list
     (list 'bolt-cutters (list 'melee-weapon 'tool))))

  (case build
    ['desperate (set-trait! (situation-pc *situation*) "constitution" 10)
                (set-actor-max-hp! (situation-pc *situation*) 4)
                (set-actor-hp! (situation-pc *situation*) 4)
                (set-trait! (situation-pc *situation*) "charisma" 10)
                (set-trait! (situation-pc *situation*) "strength" 7)]
    ['bruiser (set-trait! (situation-pc *situation*) "constitution" 10)
              (set-trait! (situation-pc *situation*) "charisma" 7)
              (set-trait! (situation-pc *situation*) "strength" 10)]
    [else (error (string-append "set-build!: unknown build type )" (symbol->string build)))]
    )

  (set-trait! (situation-pc *situation*) "exploration-skill" 1)

  (set-actor-inventory! (situation-pc *situation*) starting-inventory)
  (displayln "[Build set]")
  (character-sheet)
  )


(define *story-fragments* (make-hash))

; store a list of closed paths / trees separately instead of storing that in the fragments themselves
(serializable-struct
 story-fragment
 (id
  description
  decisions
  on-enter!))

(define (fragment id description decisions on-enter!)
  (define frag
    (story-fragment
     id
     description
     decisions
     on-enter!))
  (hash-set! *story-fragments* id frag))

(define (get-fragment id)
  (hash-ref *story-fragments* id))

; requirement is a lambda that's run on fragment's on-enter!
; on-resolve! is a lambda that's run when the decision is resolved
(serializable-struct
 decision
 (title
  description
  next-fragment
  requirement
  on-resolve!)
 #:constructor-name decision*)

(define (make-decision
         title
         description
         next-fragment
         [requirement (λ () '())]
         [on-resolve! (λ () '())])
  
  (decision* title
             description
             next-fragment
             requirement
             on-resolve!))

(define (get-modifier-string modifier)
  (cond ((negative? modifier) (number->string modifier))
        ((= 0 modifier) (number->string modifier))
        ((positive? modifier) (string-append "+" (number->string modifier)))))

; This should happen on the beginning of a life
; and with runs, you select the loadout
(fragment
 1
 "Otava has never been this far. Nobody has, nobody goes this far. But she'll make it, and she'll make it back."
 (let ([decisions '()])
   (set! decisions (append-element decisions (make-decision
                                              "Because she's desperate."
                                              "Because she's desperate.\n\nShe's running out of time. Soon she'll start losing more than just her fingers, if she cannot deliver the goods. But desperation, she knows, gives you an edge. Sharpens the senses. Makes you dangerous."
                                              'exit-and-set-build-desperate
                                              )))
   
   (set! decisions (append-element decisions (make-decision
                                              "Because she punches really hard."
                                              "She can crack a jawbone with her bare hands. That should keep her alive."
                                              'exit-and-set-build-bruiser
                                              ;(λ () (passive-check 'luck))
                                              )))
   decisions)
 (λ () (create-goal 'pay-off-debt))
 )

(fragment
 11
 "A hooded figure emerges from behind the trees. \"Those bolt cutters of yours, looking for some work? There's an old abandoned nuclear lab half a day from here. Break in, take what you want, but bring us one thing: A leatherbound book with the inscription 'Yarn of the World-Gorger'. Bring it to us. Pay you in bullets, how's 11 rounds sound?\""
 (let ([decisions '()])
   (set! decisions (append-element decisions (make-decision
                                              "Ask about the Yarn."
                                              "\"Yarn of the what?\""
                                              12
                                              (λ () (passive-check 'fail-charisma-mod '> -1 'silent))
                                              )))
   
   (set! decisions (append-element decisions (make-decision
                                              "Ask who's 'us'."
                                              "\"'Us'? Who's 'us'?\""
                                              14
                                              (λ () (passive-check 'charisma-mod '> -1))
                                              )))
   decisions)
 (λ () '())
 )

(fragment
 12
 "\"'Yarn of the World-Gorger'. It's, uh, it's a mythological book. Bound in leather, pentacle on cover. It used to belong to one of the subjects, Subject 101, he was an Adept. Not related to the work at the laboratory at all. Walk in, find his locker, grab the book, walk out, bring us the book. 11 bullets could save your life 11 times. What do you say?\""
 (list (make-decision "Agree to bring the book." "\"Okay, so tell me what you know about the laboratory.\"" 'create-quest-and-exit) ; plus a small loredump and set some knowledge or something bonus here!
       (make-decision "It's more valuable than 11 bullets. Decline and keep the book to yourself." "\"Not interested, but thanks for the chat.\"" 'exit))
 (λ () '())
 ) ; here XP reward and set the pc as 'cunning' (and figure out what that means)

(fragment
 14
 (string-append
  "\"It's... ah, wouldn't make sense to you, you are not ready yet. When you are, seek the Anthead Girl. Look, will you bring us the book or not?\""
  ) ; and drop some meta-visible info or something somewhere; create a goal?

 (let ([decisions '()])
   (set! decisions (append-element decisions (make-decision
                                              "Ask about the book."
                                              "\"The book, Yarn of the what?\""
                                              12)))
   decisions)
 (λ () (create-goal 'the-anthead))
 )

(fragment
 20
 (string-append
  "Otava is unsure whether to climb the ridges or head lower and try to follow the valleys. The ridges would perhaps mean drier feet, faster progress, and eventually better visibility if the fog dissipates. On the other hand, the laboratory ultimately lies on the banks of Martaanvuo river, and she's pretty sure that all the hollows here ultimately lead to Martaanvuo."
  )

 (let ([decisions '()])
   (set! decisions
         (append-element
          decisions
          (make-decision
           "Follow the ridges."
           "Otava decides to climb the hills and try to stay as high as possible. The fog's going to have to dissipate eventually, and then she'll get a good overview of the landscape, see at least Martaanvuo river, and maybe the laboratory she's looking for."
           (λ ()
             (begin
               (move-pc-to-location! ridges)
               (define action (make-action
                               #:symbol 'search-for-paths
                               #:actor (situation-pc *situation*)
                               #:duration 100
                               #:target '()
                               #:tags '(downtime)))

               ; 'success, 'failure or 'suspended
               (define
                 action-result
                 (resolve-pc-action! action))
                                                  
               (cond ((eq? action-result 'success)
                      (begin
                        (set-location-neighbors!
                         swamp
                         (append-element
                          (location-neighbors swamp)
                          ruins))
                        21))
                     ((eq? action-result 'interrupted)
                      (begin
                        (displayln "--interrupted")
                        'exit
                        ))
                     (else
                      (begin
                        (paragraph "After about half a day of searching, Otava still hasn't found anything remotely interesting.")
                        'exit))))))))
   
   (set! decisions
         (append-element decisions
                         (make-decision
                          "Follow the valleys."
                          "The shortest way to Martaanvuo river is also the simplest, nevermind a bit of a swamp. If she finds the river, she'll find the laboratory. And when she finds the laboratory, she'll find what she's looking for."
                          (λ ()
                            (begin
                              (move-pc-to-location! valleys)
                              (define action (make-action
                                              #:symbol 'search-for-paths
                                              #:actor (situation-pc *situation*)
                                              #:duration 100
                                              #:target '()
                                              #:tags '(downtime)))

                              ; 'success, 'failure or 'suspended
                              (define
                                action-result
                                (resolve-pc-action! action))
                                                  
                              (cond ((eq? action-result 'success)
                                     (begin
                                       (set-location-neighbors!
                                        swamp
                                        (append-element
                                         (location-neighbors swamp)
                                         ruins))
                                       23))
                                    ((eq? action-result 'interrupted)
                                     (begin
                                       (displayln "--interrupted")
                                       'exit
                                       ))
                                    (else
                                     (begin
                                       (paragraph "After about half a day of searching, Otava still hasn't found anything remotely interesting.")
                                       'exit))))))))
   decisions)
 (λ () '())
 )

(fragment
 21
 (string-append
  "After half a day of making her way eastward on the rolling ridges, Otava comes upon a hill that's steeper and taller than any thus far. The jagged silhouettes of ruined buildings looming against the gray sky look menacing, alien. There's a small pillar of smoke rising from the hilltop."
  )

 '()
 (λ () '())
 )


(fragment
 23
 (string-append
  "Success!"
  )

 (let ([decisions '()])
   (set! decisions (append-element decisions (make-decision
                                              "Nice."
                                              "Nice."
                                              'exit)))
   decisions)
 (λ () '())
 )


(fragment
 50
 (string-append
  "\"Otava, what kind of a name is that anyway? What does it mean?\""
  )
 (let ([decisions '()])
   (set! decisions (append-element decisions (make-decision
                                              "A bear."
                                              "\"It means a bear. The keeper of the forest.\""
                                              51
                                              (λ () (passive-check 'strength-mod '> -1))
                                              )))
   (set! decisions (append-element decisions (make-decision
                                              "Northstar."
                                              "\"Northstar.\""
                                              52
                                              (λ () (passive-check 'intelligence-mod '> -1))
                                              )))

   (set! decisions (append-element decisions (make-decision
                                              "Don't know."
                                              "\"Don't know.\""
                                              53)))
   decisions)
 (λ () '())
 )

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


(serializable-struct
 situation
 (world
  [pc #:mutable]
  [life #:mutable]
  [run #:mutable]
  [round #:mutable]
  [elapsed-time #:mutable]
  [in-combat? #:mutable]
  [current-fragment #:mutable]
  [goals #:mutable]
  ))

(define *situation*
  (let ([new-world (world (list edges swamp ridges valleys crematory ruins sewers cache workshop spring) 0 0)]
        [pc (make-new-pc)])
    (situation new-world pc 0 0 0 0 #f '() '())))

(define (in-combat?)
  (situation-in-combat? *situation*))


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
              (set! events (append-element events ev))))
       )))
  events
  )

(define (character-sheet)
  (define actor (situation-pc *situation*))
  (define traits (actor-traits actor))
  (define traits-list (for/list ([(k v) (in-hash traits)])
                        (if (or (eq? k "strength")
                                (eq? k "charisma")
                                (eq? k "constitution")
                                (eq? k "intelligence")
                                (eq? k "dexterity"))
                            (list (string-append " " k " ")
                                  (string-append " "
                                                 (number->string v)
                                                 " "
                                                 "[" (get-modifier-string (get-attribute-modifier-for v)) "]"
                                                 " "))
                            (list (string-append " " k " ") (string-append " " (number->string v) " ")))))
  
  (define sheet
    (list
     (list " Name " (string-append " " (actor-name actor) " "))
     (list " HP " (string-append " " (number->string (actor-hp actor)) "/" (number->string (actor-max-hp actor)) " "))
     ))
  (set! sheet (append sheet traits-list))
  (info-card
   sheet
   "Character sheet"
   )
  #t
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
; currently: goal - status - notes
; and table-display formatted
(define *goals* '())

(define (create-goal goal-symbol)
  (define goal
    (case goal-symbol
      ['pay-off-debt
       (list " pay off the debt to the Collector "
             " in progress "
             " unsettled: 4,328 grams of U-235 ")]
      ['the-anthead
       (list " seek the Anthead Girl "
             " not started "
             " \"not ready yet\" ")]))
  (set! *goals*
        (append-element *goals* goal))

  (info-card
   (list goal)
   "New goal")
  )

(define (goals)
  (define sheet
    (append
     (list
      (list " goal " " status " " notes ")
      )
     *goals*
     ))
  (info-card
   sheet
   "Goals")
  )

(define (current-location)
  #;(displayln "-- current-location: TODO move to situation")
  (actor-current-location (situation-pc *situation*)))

(define (take-damage actor damage)
  (when (< damage 0) (error "take-damage: damage cannot be less than 0"))
  (define new-hp (- (actor-hp actor) damage))
  (when (< new-hp 0) (set! new-hp 0))
  (set-actor-hp! actor new-hp)
  (if (= 0 (actor-hp actor))
      'dead
      'hit))

(define (alive? actor)
  (> (actor-hp actor) 0))

; TODO shit
(define (get-next-npc-action actor)
  (if (not (in-combat?))
      (make-action #:symbol 'hold-at-gunpoint
                   #:actor actor
                   #:duration 1
                   #:target 'pc
                   #:tags '(delayed-resolution))
      (make-shoot-action actor))
  
  )


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

(define (move-actor-to-location! actor location)
  (remove-actor-from-its-current-location! actor)
  (set-actor-current-location! actor location)
  (add-actor-to-location! location actor))

(define (get-current-enemies)
  (filter
   (λ (actor) (and (alive? actor)
                   (not (pc-actor? actor))))
   (location-actors (current-location))))

(define (in-range? target attack-mode)
  (case attack-mode
    ['melee #t]
    [else (displayln "in-range? not implemented yet for this attack mode")]))

(define (get-go-to-text-from-location-to-another from-type to-type)
  (case to-type
    ['ruins "Climb the hill to the ruins."]
    ['swamp "Enter the swamps."] ; TODO: Toggle meta-progression on when the swamps are entered for the first time
    ['edges "Go back to Edgeflats."]
    [else (string-append "Go to " (symbol->string to-type) ".")]))

(define (get-world-choices world actor)
  (cond ((in-combat?)
         (get-combat-choices world actor))
        ((eq? (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*))) 'night)
         (get-nighttime-choices world actor))
        (else (get-downtime-choices world actor))))

(define (get-combat-choices world actor)
  (define combat-choices '())
  (define targets (get-current-enemies))
  (for ([i (in-range 0 (length targets))])
    (define target (list-ref targets i))
    '()
    )
  (displayln "get-combat-choices: TODO not implemented yet")
  combat-choices
  )

(define (get-nighttime-choices world actor)
  (displayln "get-night-time-choices: TODO not implemented yet")
  (list
   (make-choice
    'sleep
    "Sleep." 
    (λ () (make-action
           #:symbol 'sleep
           #:actor (situation-pc *situation*)
           #:duration 100
           #:target '()
           #:tags '())))))

(define (get-location-name-from-location-type location-type)
  (cond ((eq? location-type 'swamp) "the swamps")
        (else (string-append "get-location-name-from-location-type: unknown location type: " (symbol->string location-type)))))

(define (get-continue-pending-action-name pending-action)
  (cond ((eq? (action-symbol pending-action) 'go-to-location)
         (string-append
          "Continue towards "
          (get-location-name-from-location-type (location-type (action-target pending-action)))
          "."))
        ((eq? (action-symbol pending-action) 'search-for-paths)
         (string-append
          "Keep on searching for paths."))
        (else (string-append "get-continue-pending-action-name: unknown action symbol: " (symbol->string (action-symbol pending-action))))))

; This should be refactored
; - a big question is, where does much of this logic
; best fit? locations?
(define (get-downtime-choices world actor)
  (define pending-choices '())
  (when (not (null? *pending-action*))
    (set!
     pending-choices
     (list
      (make-choice
       'go-to-location
       (get-continue-pending-action-name *pending-action*)
       (λ ()
         (begin0
           *pending-action*
           (set! *pending-action* '())))))))
  ;(displayln "PC-not-null")

  
  (define change-location-choices '())
  (define downtime-choices '())
  (when (and (not (in-combat?))
             (not (location-has-tag? (current-location) 'forbid-simple-exit)))
    (cond ((eq? (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*))) 'night)
           '()))
    (define neighbors
      (location-neighbors (current-location)))
    (for ([i (in-range 0 (length neighbors))])
      (define neighbor (list-ref neighbors i))
      (set! change-location-choices
            (append change-location-choices
                    (list
                     (make-choice
                      'go-to-location
                      (get-go-to-text-from-location-to-another (location-type (current-location)) (location-type neighbor)) 
                      (λ () (make-action
                             #:symbol 'go-to-location
                             #:actor (situation-pc *situation*)
                             #:duration 100
                             #:target neighbor
                             #:tags '(downtime))))))))

    (set! downtime-choices
          (if (eq? (location-type (current-location)) 'swamp)
              (list
               (make-choice
                'forage
                (string-append "Forage.")
                (λ () (make-action
                       #:symbol 'forage
                       #:actor (situation-pc *situation*)
                       #:duration 100
                       #:target '()
                       #:tags '(downtime)))))
              '())


          ))

  (define end-run-choices '()) ; poor name
  (when (eq? (location-type (current-location)) 'edges)
    (set! end-run-choices
          (list
           (make-choice
            'go-back-to-the-shack
            "Head back to The Shack."
            (λ () (make-action
                   #:symbol 'end-run
                   #:actor (situation-pc *situation*)
                   #:duration 0
                   #:target '()
                   #:tags '(downtime)))))))
  (when (eq? (location-type (current-location)) 'spring)
    (set! end-run-choices
          (list
           (make-choice
            'dive-in-spring
            "Dive in the spring."
            (λ () (make-action
                   #:symbol 'win-game
                   #:actor (situation-pc *situation*)
                   #:duration 0
                   #:target '()
                   #:tags '(downtime)))))))

  
  (define neighbors
    (location-neighbors (current-location)))

  (define location-specific-choices
    (for/list ([action (location-actions-provided (current-location))])
      (case action
        ['search-for-paths
         (make-choice
          'search-for-paths
          "Search for paths."
          (λ () (make-action
                 #:symbol 'search-for-paths
                 #:actor (situation-pc *situation*)
                 #:duration 100
                 #:target '()
                 #:tags '(downtime))))])))
  (define choices-before-pruning
    (append pending-choices change-location-choices downtime-choices end-run-choices location-specific-choices))

  (define (show-choice-based-on-pending-choice? choice)
    (cond ((not (null? pending-choices))
           #f)
          (else
           #t)))
  
  (define
    pruned-choices
    (filter
     show-choice-based-on-pending-choice?
     choices-before-pruning))
  (define all-choices
    (append pending-choices pruned-choices))
  all-choices)

(define (time-of-day-from-jiffies jiffies)
  (define jiffies-of-current-day (remainder jiffies 400))
  (define time-of-day
    (cond ((< jiffies-of-current-day 100) 'morning)
          ((< jiffies-of-current-day 200) 'afternoon)
          ((< jiffies-of-current-day 300) 'evening)
          ((< jiffies-of-current-day 400) 'night)))
  time-of-day
  )

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
     (list " current location id "
           (string-append
            " "
            (number->string (location-id (current-location)))
            " "))
     (list " time of day " (string-append " " (symbol->string (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*)))) " "))
     (list " elapsed time (total) " (string-append " " (number->string (world-elapsed-time (situation-world *situation*))) " "))
     ))
  #;(info-card round-summary (string-append "Begin round " (number->string (situation-round *situation*))))
  
  (set! action-queue '())
  #; (when (not (eq? '() current-encounter)) (send current-encounter on-begin-round!))
  (when (not (null? (situation-current-fragment *situation*)))
    (current-fragment-on-begin-round!))
  )

(define (add-to-action-queue action)
  (set! action-queue (cons action action-queue)))
(define (remove-from-action-queue actions)
  (set! action-queue (remq* actions action-queue)))

(define (sort-action-queue)
  (set! action-queue (sort
                      action-queue
                      action-faster-than?))
  
  )

(define (enqueue-npc-actions)
  (define actors (location-actors (current-location)))
  (for ([actor actors])
    (when (not (pc-actor? actor))
      (define next-action (get-next-action actor))
      (add-to-action-queue next-action))))



(define (update-npc-reactions pc-action)
  (define npcs (get-current-enemies))
  (when (aggressive? pc-action)
    ; remove own actions from queue
    (for ([actor npcs])
      (define actions (filter
                       (λ (action) (eq? actor (action-actor action)))
                       action-queue))
      (remove-from-action-queue actions)
      ; blam blam
      (define action (make-shoot-action actor))
      (add-to-action-queue action))
    )
  )

(define (make-shoot-action actor)
  (define action (make-action
                  #:symbol 'shoot
                  #:actor actor
                  #:duration 1
                  #:target (situation-pc *situation*)
                  #:tags '(combat delayed-resolution)))
  action)

(define (serialize-state)
  ; prng can be stored as vector:
  ; https://docs.racket-lang.org/reference/generic-numbers.html#%28def._%28%28quote._~23~25kernel%29._pseudo-random-generator-~3evector%29%29
  '())

(define (serialize-input)
  '())

(define encounter<%>
  (interface () on-begin-round! on-end-round! on-get-pc-action! get-encounter-choices))

(define scavenger-encounter%
  (class* object% (encounter<%>)
    (field [encounter-nodes '()])
    (field [current-node '()#;(car encounter-nodes)])
    (super-new)

    (define scavenger
      (make-actor
       "scavenger"
       4
       '()))
      
    
    (define/private (exit-encounter!)
      (displayln "The scavenger disappears.")
      (newline)
      (remove-actor-from-its-current-location! scavenger)
      'exit-encounter)

    (define/public (begin-encounter!)
      (move-actor-to-location! scavenger (current-location)))

    (define/public (on-begin-round!)
      '())

    (define/public (on-end-round!)
      (define current-enemies (get-current-enemies))
      (if (= (length current-enemies) 0)
          (exit-encounter!)
          '()))

    (define/public (on-get-pc-action! pc-action)
      '())

    (define/public (get-encounter-choices)
      '())
    ))

(define (describe-situation)
  '()
  )

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

(define (wait-for-confirm)
  (newline)
  (displayln "[Enter]")
  (newline)
  (define input (read-line))
  input)

(define (info-card content title)
  (when (not (null? title)) (displayln (string-append "[" title "]")))
  (print-table content #:row-sep? #f)
  (newline))

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

(define (luck-check)
  (define wangernumbs
    (list
     "i"
     "8-4"
     "21.3"
     ".01"
     "41"
     "9Ɛ"
     "12"
     "70"
     "26"
     "2"
     "-3"
     "±6"))
  (define wanger-index (random (length wangernumbs)))
  (define result (if (< wanger-index (/ (length wangernumbs) 2))
                     #t
                     #f))
  (define result-text (if result
                          " success "
                          " failure "))
  (define wangernumb (list-ref wangernumbs wanger-index))
  (info-card
   (list
    (list " luck " (string-append " " wangernumb " "))
    (list " " result-text))
   "Luck check")
  result)

(define (passive-check type comparator target-number . silent)
  (define text "")
  (case type
    ['charisma-mod (set! text (string-append "charisma mod > " (number->string target-number)))]
    ['fail-charisma-mod (set! text (string-append "fail charisma mod > " (number->string target-number)))]
    [else (error (string-append "passive check: unknown type: " (symbol->string type)))])

  (define attribute-value (get-trait (situation-pc *situation*) "charisma"))
  (define modifier (get-attribute-modifier-for attribute-value))
  (define successful? (> modifier target-number))

  ; dirty but eh: for failures, flip successful here
  (case type
    ['fail-charisma-mod (set! successful? (not successful?))]
    )
  
  (define result (if successful?
                     "check passed"
                     "check failed"))
  (define sheet
    (list
     (list (string-append " " text " ")
           (string-append " " (number->string attribute-value) " (" (get-modifier-string modifier) ") ")
           (string-append " " result " "))
     
     ))
  (when (null? silent)
    (info-card
     sheet
     "Passive check"
     ))
  successful?)

; returns boolean
(define (skill-check title bonus target-number)
  (define first-d (d 1 6))
  (define second-d (d 1 6))
  (define roll-total (+ first-d second-d bonus))
  (define successful? (>= roll-total target-number))
  (define success-string
    (if successful?
        ", success"
        ", failure"))
  (define results
    (list
     (list " 2d6 + skill " " >= " " location TN ")
     (list
      (string-append
       " "
       (number->string first-d)
       "+"
       (number->string second-d)
       "+"
       (number->string bonus)
       " = "
       (number->string roll-total))
      " >= "
      (string-append " " (number->string target-number) success-string " "))))
               
  (info-card
   results
   (string-append "Skill check: " title))

  successful?)

(define (resolve-shoot-action! action)
  (define actor (action-actor action))
  (define target (action-target action))
  
  #;(define target-number (actor-defense-number target))
  (displayln "resolve-shoot-action!: TODO reimplement")
  (define target-number 7)
  
  #;(define success? (skill-check "Shoot" (actor-attack-skill actor) target-number))
  #;(define damage-roll ((actor-attack-damage actor)))
  (define success? #t)
  (define damage-roll 2)

  (when success?
    (info-card
     (list
      (list " damage roll formula " " result ")
      (list
       " < see actor > "
       (string-append
        " "
        (number->string damage-roll))
       ))
     "HP damage roll"))

  (define action-result 'ok)
  (when success? (set! action-result (take-damage target damage-roll)))

  (actor-status-card target (actor-name target))
  (newline)

  action-result
  )

(define (handle-exploration-check-result! result)
  (if result
      (begin
        (expose-neighbor! (current-location))
        'successful)
      (begin
        (displayln "Exploration failed.")
        'failure)))

; can return:
; 'pc-dead  when the pc is dead as a consequence of this action
; 'ok       when the action is completely resolved and not explicitly successful or failed
; 'success  when the action is completely resolved and explicitly successful
; 'failed   when the action is completely resolved and fails
(define (resolve-action! action)
  (when (alive? (action-actor action))
    (cond ((or (eq? (action-symbol action) 'shoot)
               (eq? (action-symbol action) 'melee))
           (define result (resolve-shoot-action! action))
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
          (else (error (string-append "resolve-action!: unknown action type " (symbol->string (action-symbol action))))))))

(define (spawn-encounter)
  (displayln "-- spawn-encounter disabled")
  #;(when (eq? '() current-encounter)
      (begin
        (set! current-encounter (new scavenger-encounter%))
        (send current-encounter begin-encounter!)
        )))

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

(define (move-pc-to-location! location)
  ; TODO: location on-exit / on-enter triggers here
  #;(displayln (string-append "-- move-pc-to-location!: moving to " (~v location)))
  (remove-actor-from-location! (current-location) (situation-pc *situation*))
  (set-actor-current-location! (situation-pc *situation*) location)
  (add-actor-to-location! location (situation-pc *situation*)))

(define *pending-action* '())

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
    (else (displayln (string-append "narrate-event: unknown event type "
                                    (symbol->string (event-type event)))))))

; may return:
; void
; 'end-run
; 'win-game
; 'interrupted
; 'success
; 'failure
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

                   (when (eq? (timeline-metadata timeline) 'interrupted)
                     (handle-pc-action-interrupted! timeline)
                     (return 'interrupted))
    
    
                   ; should check results
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
                   (when (or (eq? action-result 'successful)
                             (eq? action-result 'failure))
                     action-result)
                   ))

  ; do the state management mutation stuff
  (when (eq? 'interrupted result)
    (define time-left (- (action-duration action) elapsed-time))
    (set! *pending-action* (lens-set action-duration-lens action time-left)
          #;(pending-action action time-left))
    (info-card
     (list
      (list
       (string-append " "
                      (symbol->string (action-symbol *pending-action*))
                      " ")
       (string-append " "
                      "time left: "
                      (number->string (action-duration *pending-action*))
                      " ")))
     "Pending action"))
  result
  )

(define (handle-interrupting-event! event)
  (cond ((eq? (event-type event) 'spawn-enemies)
         (displayln "SPAWNING ENEMIES"))
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
         (displayln "EVENT (type details at):")
         (displayln (event-type event))
         (displayln (event-details event))
         (displayln (event-at event))
         (handle-interrupting-event! event)
         )
        (else
         (displayln "handle-pc-action-interrupted!: unexpected amount of interrupting events")))
  )

(define (resolve-npc-action! action)
  (resolve-action! action))

(define (resolve-turns!)
  (let/ec end-round-early
    (for ([action action-queue])
      (define turn-result (resolve-turn! world action))
      (when (eq? turn-result 'pc-dead) (end-round-early))
      )
    ))

(define (resolve-turn! world action)
  (if (pc-actor? (action-actor action))
      (resolve-pc-action! action)
      (resolve-npc-action! action))
  )

(define (end-encounter)
  (displayln "-- end-encounter disabled")
  #;(set! current-encounter '()))

(define (on-end-round)
  (define current-enemies (get-current-enemies))
  (when (= (length current-enemies) 0)
    '()
    #;(displayln "-- on-end-round: fix (in-combat?)")
    #;(set! in-combat? #f))
  (when (not (null? (situation-current-fragment *situation*)))
    (current-scene-on-end-round!)) ; TODO scene-rounds should maybe not increase round?
  #;(when (not (eq? '() current-encounter))
      (define encounter-status (send current-encounter on-end-round!))
      (when (eq? 'exit-encounter encounter-status) (end-encounter)))

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
  
           #;(when (not (null? current-encounter))
               (define encounter-status (send current-encounter on-get-pc-action! pc-action))
               (when (eq? 'exit-encounter encounter-status) (end-encounter)))

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
           (when (not (alive? (situation-pc *situation*))) (set! round-exit-status 'pc-dead))
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
  (displayln "menu")
  #t)

(define (wait-for-input)
  (define input (read-line))
  (newline)
  input)

(define (get-meta-commands-with-keys)
  (define meta-commands (make-hash))
  (hash-set! meta-commands "Q" (cons "[Q]: Quit." quit))
  (hash-set! meta-commands "D" (cons "[D]: Describe situation again." describe-situation))
  (hash-set! meta-commands "M" (cons "[M]: Menu." menu))
  (hash-set! meta-commands "C" (cons "[C]: Character sheet." character-sheet))
  (hash-set! meta-commands "I" (cons "[I]: Inventory." inventory))
  (hash-set! meta-commands "G" (cons "[G]: Goals." goals))
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
  (move-actor-to-location! (situation-pc *situation*) edges)
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

(define (on-begin-life)
  (set-situation-life! *situation* (add1 (situation-life *situation*)))
  (set-situation-pc! *situation* (make-new-pc))
  (define life-info
    (list
     (list " life " (string-append " " (number->string (situation-life *situation*)) " "))
     ))
     
  (info-card life-info (string-append "Begin life number " (number->string (situation-life *situation*))))
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