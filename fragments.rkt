#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "action.rkt")
(require "checks.rkt")
(require "location.rkt")
(require "fragment.rkt")
(require "io.rkt")
(require "situation.rkt")
(require "utils.rkt")
(require "world.rkt")

(lazy-require
 ["martaanvuo.rkt"
  (engine-function
   pc
   in-combat?
   actor-in-range?
   set-in-combat?!
   move-actor-to-location!
   move-pc-to-location!
   resolve-pc-action!)])



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
 (λ () (create-quest 'pay-off-debt))
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
  ) ; and drop some meta-visible info or something somewhere; create a quest?

 (let ([decisions '()])
   (set! decisions (append-element decisions (make-decision
                                              "Ask about the book."
                                              "\"The book, Yarn of the what?\""
                                              12)))
   decisions)
 (λ () (create-quest 'the-anthead))
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
                        (set-location-neighbors!
                         swamp
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



