#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "action.rkt")
(require "checks.rkt")
(require "location.rkt")
(require "fragment.rkt")
(require "io.rkt")
(require "item.rkt")
(require "pc.rkt")
(require "situation.rkt")
(require "utils.rkt")
(require "world.rkt")

(lazy-require
 ["round-resolver.rkt"
  (resolve-pc-action!)])



(define *story-fragments* (make-hash))

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

; This should happen on the beginning of a life
; and with runs, you select the loadout
(fragment
 1
 "It is worth it, and she will make it back."
 (list
  (make-decision
   #:title "Because she's got a gun."
   #:description "Her trusty AIX revolver should keep her alive. From before the Rains, before the Cataclysm, before the whatever the fuck you want to call it happened."
   #:on-resolve! (proc (set-build! 'gun) (wait-for-confirm))
   #:next-fragment 'exit
   )

  (make-decision
   #:title "Because she punches really hard."
   #:description "She can crack a jawbone with her bare hands. That should keep her alive."
   #:on-resolve! (proc (set-build! 'bruiser) (wait-for-confirm))
   #:next-fragment 'exit)
  )
 (nop))


(fragment
 11
 "A hooded figure emerges from behind the trees. \"Those bolt cutters of yours, looking for some work? There's an old abandoned nuclear lab half a day from here. Break in, take what you want, but bring us one thing: A leatherbound book with the inscription 'Yarn of the World-Gorger'. Bring it to us. Pay you in bullets, how's 11 rounds sound?\""

 (list
  (make-decision
   #:requirement   (λ () (passive-check 'fail-charisma-mod '> -1 'silent))
   #:title         "Ask about the Yarn."
   #:description   "\"Yarn of the what?\""
   #:next-fragment 12)

  (make-decision
   #:requirement   (λ () (passive-check 'charisma-mod '> -1))
   #:title         "Ask who's 'us'."
   #:description   "\"'Us'? Who's 'us'?\""
   #:next-fragment 14))
 
 (λ () '()))

(fragment
 12
 "\"'Yarn of the World-Gorger'. It's, uh, it's a mythological book. Bound in leather, pentacle on cover. It used to belong to one of the subjects, Subject 101, he was an Adept. Not related to the work at the laboratory at all. Walk in, find his locker, grab the book, walk out, bring us the book. 11 bullets could save your life 11 times. What do you say?\""
 (list
  (make-decision
   #:title         "Agree to bring the book."
   #:description   "\"Okay, so tell me what you know about the laboratory.\""
   #:next-fragment 'create-quest-and-exit)

  ; here XP reward and set the pc as 'cunning' (and figure out what that means)
  (make-decision
   #:title         "The book's more valuable than 11 bullets. Decline and keep the book to yourself."
   #:description   "\"Not interested, but thanks for the chat.\""
   #:next-fragment 'exit))
 (λ () '())) 

(fragment
 14
 (string-append
  "\"It's... ah, wouldn't make sense to you, you are not ready yet. When you are, seek the Anthead Girl. Look, will you bring us the book or not?\""
  ) ; and drop some meta-visible info or something somewhere; create a quest?

 (make-decision
  #:title "Ask about the book."
  #:description "\"The book, Yarn of the what?\""
  #:next-fragment 12)
 (λ () (create-quest 'the-anthead))
 )

(fragment
 20
 (string-append
  "Otava is unsure whether to climb the ridges or head lower and try to follow the valleys. The ridges would perhaps mean drier feet, faster progress, and eventually better visibility if the fog dissipates. On the other hand, the laboratory ultimately lies on the banks of Martaanvuo river, and she's pretty sure that all the hollows here ultimately lead to Martaanvuo."
  )

 (list
  (make-decision
   #:title "Follow the ridges."
   #:description "Otava decides to climb the hills and try to stay as high as possible. The fog's going to have to dissipate eventually, and then she'll get a good overview of the landscape, see at least Martaanvuo river, and maybe the laboratory she's looking for."
   #:next-fragment (λ ()
                     (begin
                       (move-pc-to-location! blackfang-peak)
                       (define action (make-action
                                       #:symbol 'search-for-paths
                                       #:actor (pc)
                                       #:duration 100
                                       #:target '()
                                       #:tags '(downtime)
                                       #:details '()))

                       ; 'success, 'failure or 'suspended
                       (define
                         action-result
                         (resolve-pc-action! action))
                                                  
                       (cond ((eq? action-result 'success)
                              (begin
                                (displayln "TODO: Fix this post-location-rewrite")
                                #;(set-location-neighbors!
                                 martaanvuo-swamp
                                 (append-element
                                  (location-neighbors swamp)
                                  ruins))
                                21))
                             ((eq? action-result 'interrupted)
                              (begin
                                'exit
                                ))
                             (else
                              (begin
                                (paragraph "After about half a day of searching, Otava still hasn't found anything remotely interesting.")
                                'exit)))))))

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

 (list (make-decision
        #:title "Nice."
        #:description "Nice."
        #:next-fragment 'exit))
 (λ () '())
 )


(fragment
 50
 (string-append
  "\"Otava, what kind of a name is that anyway? What does it mean?\""
  )
 (list
  (make-decision
   #:title "A bear."
   #:description "\"It means a bear. The keeper of the forest.\""
   #:next-fragment 51
   #:requirement (λ () (passive-check 'strength-mod '> -1))
   )
  (make-decision
   #:title "Northstar."
   #:description "\"Northstar.\""
   #:next-fragment 52
   #:requirement (λ () (passive-check 'intelligence-mod '> -1))
   )
  (make-decision
   #:title "Don't know."
   #:description "\"Don't know.\""
   #:next-fragment 53))
 
 
 (λ () '())
 )

(fragment
 100
 (string-append
  "[post-combat steps to do]"
  )
 (list (make-decision
        #:title "Catch some breath."
        #:description "Otava catches some breah."
        #:next-fragment 'exit
        ))
 (λ () '())
 )

(fragment
 200
 (string-append
  "A complicated tangle of wires and pipes fill much of the back half of the room. On a cluttered desk to the side there's a pile of schematics and notes."
  "\n\n"
  "Hartmann Device mk. II."
  "\n\n"
  "The sequence to power on the device is described on a series of handwritten notes scribbled in the margin of one of the myriad of the schematics."
  )
 (list
  (make-decision
   #:title "Power on the device."
   #:description "Otava dives through the jungle of cables and pipes, connecting what needs to be connected, turning on what needs to be turned on. After an hour of work, the device finally comes to life. Otava flicks the switch to begin the process, and a temperature gauge starts plummeting.\n\nAs soon as the temperature inside the kernel chamber of the device reaches point-triple-zero-one Kelvin, the zero-point field within falls to a lower state of energy, commencing a chain reaction proceeding at the speed of light from the kernel outwards. As the substratum of physical existence unfolds, matter and energy and time and space irreversibly cease to exist. Otava blinks out of existence along with the rest of the universe, never to be born again."
   #:next-fragment 'exit
   #:on-resolve! (λ () (end-game))
   )

  (make-decision
   #:title "Leave the device be."
   #:description "Otava leaves Hartmann Device mk. II alone, wondering what might have been."
   #:next-fragment 'exit
   ))
 
 (λ () '())
 )

(fragment
 300
 (string-append
  "There's an inconspicuous sign saying 'Murkwater Aix' on the wall of an automated guard's booth.")
 (list
  (make-decision
   #:title ""
   #:description ""
   #:next-fragment 'exit
   ))
 
 (λ () '())
 )

