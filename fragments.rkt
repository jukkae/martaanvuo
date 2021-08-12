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
(require "quests.rkt")
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
 "Still, someone's been there, and so will she. And she'll make it back, too."
 (list
  (make-decision
   #:title "She has a gun."
   #:description "Her Aegis Metalworks revolver will get her out of a pinch. One of the last ones they ever made before the Rains. Hundreds of years old, and more reliable than anything that's made since."
   #:on-resolve! (proc
                  (set-build! 'gun)
                  (wait-for-confirm)
                  (paragraph "The trail goes past some jagged pieces of metal that stand up from the ground. This is anomaly perimeter, then – she's getting close.")
                  (paragraph "The trail turns behind a boulder and comes to a fork. A fork? Broker had said nothing about a fork.")
                  (paragraph "The left branch turns into a climb up a rocky hill. A magpie's call echoes from somewhere up the hill. An army of ants is marching down the other branch, toward what must be Martaanvuo swamp."))
   #:next-fragment 'exit
   )

  (make-decision
   #:title "She punches really hard."
   #:description "She can crack a jawbone with her bare hands. The trick is in hitting from the side and from below."
   #:on-resolve! (proc
                  (set-build! 'bruiser)
                  (wait-for-confirm)
                  (paragraph "The trail goes past some jagged pieces of metal that stand up from the ground. This is anomaly perimeter, then – she's getting close.")
                  (paragraph "The trail turns behind a boulder and comes to a fork. A fork? Broker had said nothing about a fork.")
                  (paragraph "The left branch turns into a climb up a rocky hill. A magpie's call echoes from somewhere up the hill. An army of ants is marching down the other branch, toward what must be Martaanvuo swamp."))
   #:next-fragment 'exit)

  (make-decision
   #:title "She's a survivor."
   #:description "She is a survivor. That's what's kept her alive all these years."
   #:on-resolve! (proc
                  (set-build! 'survivor)
                  (wait-for-confirm)
                  (paragraph "The trail goes past some jagged pieces of metal that stand up from the ground. This is anomaly perimeter, then – she's getting close.")
                  (paragraph "The trail turns behind a boulder and comes to a fork. A fork? Broker had said nothing about a fork.")
                  (paragraph "The left branch turns into a climb up a rocky hill. A magpie's call echoes from somewhere up the hill. An army of ants is marching down the other branch, toward what must be Martaanvuo swamp."))
   #:next-fragment 'exit)
  )
 (λ () '()#;(set-prompt! "Because...")))




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
  "Otava thinks the magpie should be close, but the sound seems to come from a slightly different direction every time."
  )

 (list (make-decision
        #:title "Listen quietly."
        #:description "The magpie laughs straight above her. The bird is hidden somewhere within the shadowy branches of a large dead oak. There's a worn tombstone half buried under the roots of the tree."
        #:on-resolve! (proc
                       (remove-feature-from-location! (current-location) 'magpie-effigy))
        #:next-fragment 'exit)
       (make-decision
        #:title "Listen quietly, with a gun in the hand."
        ; TODO this should be The Eternal Bullet, which would have a chance of not being consumed, and a chance of seriously messing with the ontology of the world
        #:description "There's a flutter. The outline of a tree shakes as the magpie takes flight and disappears, unseen. Otava jogs the rest of the way. At the bottom of the tree there are the decaying remains of a frog. Ants have created a temporary bypass around the rotting carcass. Something glimmery catches Otava's eyes."
        #:on-resolve! (proc
                       (paragraph "Otava picks up a revolver cartridge. Ordinary-looking, and the right size.")
                       (set-flag 'eternal-bullet)
                       (add-ammo! 1)
                       (remove-feature-from-location! (current-location) 'magpie-effigy))
        #:next-fragment 'exit))
 (λ () '())
 )




(fragment
 100
 (string-append
  "[post-combat steps to do]"
  )
 (list (make-decision
        #:title "Exit action."
        #:description "Combat finished."
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
 'stiltman-dialogue
 (string-append
  "Stiltman goes quiet and seems to struggle against an unseen wind, Otava thinks, or like an animal that's tied down struggling to break free.")
 (list
  (make-decision
   #:title "\"Uh...\""
   ; this is starting to look like the fragment should contain calls to paragraph directly
   #:description
   (string-append
    "\"I, uh...\", Otava begins. \"– Thank the heavens that you noticed me!\", the man says, and continues \"I have only a little of – do not read it! – time, as I am currently unstably present, due to an accident at the Murkwater / Aegis complex downriver. You can call me... Stiltman.\""
    "\n\n"
    "\"Do not read it! –\", Stiltman goes, \"– I was fishing, thank the heavens you noticed me – \", and then he's stable again. \"– I was fishing, thank the heavens you noticed me – so they were manufacturing a special kind of radioactive gold, who knows what for – I'm just a technician, see – \""
    "\n\n"
    "Otava takes a step closer. \"No! Don't come any closer or it'll get you too!\", Stiltman says, and then continues, \"They had enhanced the process, you see, gotten outside help.\""
    "\n\n"
    "\"What kind of outside help?\""
    "\n\n"
    "\"I don't know the precise details, but there was this book... Pulverization Monograph, it was called, it's like really old, all philosophy and like metaphysical woo-woo. So then they claimed that they'd got a new kind of mathematics from there, which they then used to essentially create Gold-198 out of thin air! So you see, it's not that they could convert something to the needed special radioactive gold, it's that they, well, you know. It sounds mad, I know, but we ran the simulations, everything checked out, we were well within parameter range even in the worst case, so we modified the reactor. – It's getting worse again – – I was fishing, thank the heavens you noticed me – \" Stiltman exclaims, as he looks like gravity is pulling him further out, away from the shore. \"– thank you – I am currently unstably present –\", he goes, as he disappears in the mist."
    )
   #:next-fragment 'exit
   )
  ; -> the mission is to find the monograph
  (make-decision
   #:title "Ask about the Monograph."
   #:description "\"The Monograph?\", Otava asks."
   #:next-fragment 'exit
   )
  ; -> the mission is to destroy the monograph
  (make-decision
   #:title "Ask about what the fee is for."
   #:description "\"The fee, what is it for?\""
   #:next-fragment 'exit
   ))
 
 (λ () '())
 )



