#lang racket

(provide (all-defined-out))

(require "io.rkt")
(require "locations/location.rkt")
(require "locations/locations.rkt")
(require "pc.rkt")
(require "quest.rkt")
(require "quests.rkt")
(require "round-resolver/round-resolver.rkt")
(require "state/state.rkt")
(require "utils.rkt")
(require "world.rkt")

(require "state/logging.rkt")


; content should be provided "somewhere"
; content is game-specific, not engine stuff
; but figure out where this should be called from
; engine / run-resolver
(define (narrate-begin-run #:suppress-new-chapter? [suppress-new-chapter? #f])
  (when (not suppress-new-chapter?) (next-chapter!))
  
  ; Don't show this until the second run!
  (when (not (= 1 (current-run)))
    (info-card
     (list
      (list " run " (string-append " " (number->string (current-run)) " ")))
     (string-append "Begin run number " (number->string (current-run)))))

  
  (case (current-run)
    [(1)
     (p "Otava is following an old, overgrown trail through foggy woods. The air is thick with a damp, musty smell. Broker's directions have been correct thus far.")
     (when (not (quest-exists? 'pay-off-debt))
       (create-quest 'pay-off-debt))]
    #;[(2)
       (p "\"How many more,\" she thinks as she goes down the path toward Perimeter, \"can I do?\"")]))

(define (narrate-begin-recurse-run)
  (next-chapter!)

  (p "Otava is again following the Broker's trail through the foggy woods. Better prepared, she knows what's ahead. She gets to Fork and Anthill."))
  ; Otava is on Brokerstrail and comes to Fork-and-Anthill BUT THIS TIME KNOWY OF HARTMAN-DEVICE
  ; sort of like "worlds" 1-2-3


; engine / run-resolver
(define (on-begin-run #:suppress-new-chapter? [suppress-new-chapter? #f])
  (current-run (add1 (current-run)))
  (current-round 0)
  (remove-flag 'ending-run-allowed)
  (move-pc-to-location! (find-place-by-id (world-places (current-world)) 'perimeter))
  (narrate-begin-run #:suppress-new-chapter? suppress-new-chapter?)
  )

(define (on-begin-recurse-run)
  (current-run (add1 (current-run)))
  #;(current-round 0)
  (remove-flag 'ending-run-allowed)
  (add-feature-to-location! (find-place-by-id (world-places (current-world)) 'martaanvuo-docks) 'stiltman)
  (move-pc-to-location! (find-place-by-id (world-places (current-world)) 'perimeter))
  (narrate-begin-recurse-run)
  )

(define (on-continue-run)
  '()
  )

(define (narrate-restart)
  (p
   (take-random ; TODO: probabilitify these kinds of take-random calls
    (list
     "In the depths of THE MAW the Heart of the World stops. Then, it's all black."
     "In the depths of Murkwater-Aegis Workshop, Hartmann Device is turned on. False vacuum begins its collapse and space and time unfold until there is nothing."
     (string-append "The end."
                    "\n\n\n"
                    "M A R T A A N V U O"
                    "\n"
                    "==================="
                    "\n\n"
                    "Jukka Eerikäinen (2021)"
                    "\n\n")
     "[details omitted – 3 days later] Having passed  Martaanvuo, Otava comes upon an unnamed mountain range. She crosses over and begins a new life herding reindeer. She lives the rest of her days free from suffering and dies of natural causes at an elderly age."
     ))))


(define (on-end-run exit-status)
  (when (and (not (eq? exit-status 'restart))
             (not (eq? exit-status 'recurse)))
    (cond ((> (pc-gold-amount) 0)
           (define debt-quest (find-quest 'pay-off-debt))
           (define gold-collected (pc-gold-amount))
           (reduce-debt-by! gold-collected)
           (remove-item! 'gold)
         
         

           (displayln "Quest:")
           (displayln debt-quest)
         
           (info-card
            (list
             (list " run "
                   (string-append " " (number->string (current-run)) " "))
             (list " gold collected "
                   (string-append " " (number->string (pc-gold-amount)) " grams "))
             (list " debt still owed "
                   (string-append " " (number->string (quest-details debt-quest)) " grams ")))
            (string-append "Run number " (number->string (current-run)) " ended")))

        
          (else
           (notice (string-append
                      "End run number "
                      (number->string (current-run))
                      " [failed]")))))

  
  
  (case exit-status
    ['end-run
     (p "She's still alive.")]
    ['recurse
     (define title-string
       (string-append "M A R T A A N V U O"
                      "\n"
                      "==================="))
     (p title-string)]
    ['restart
     (narrate-restart)]
    ['pc-dead '()]
    [else
     (p "narrate-end-run: unhandled exit status: " (symbol->string exit-status))])

  (when (not (eq? exit-status 'pc-dead))
    (wait-for-confirm)
    )
  )

; engine / run-resolver
(define (resolve-run mode 
                     #:suppress-new-chapter? [suppress-new-chapter? #f])

  (case mode
    ['continue (on-continue-run)]
    ['begin (on-begin-run #:suppress-new-chapter? suppress-new-chapter?)]
    ['restart (on-begin-run #:suppress-new-chapter? suppress-new-chapter?)]
    ['recurse (on-begin-recurse-run)]
    [else (dev-note "Unknown resolve-run-mode!") (error "fix this")])
  
  (define run-exit-status
    (let/ec end-run

      
      (when (eq? mode 'continue)
        (define first-round-exit-status (resolve-round 'continue))
        ; end run?
        (when (eq? first-round-exit-status 'pc-dead) (end-run 'pc-dead))
        (when (eq? first-round-exit-status 'win-game) (end-run 'win-game))
        (when (eq? first-round-exit-status 'end-run) (end-run 'end-run))
        (when (eq? first-round-exit-status 'recurse) (end-run 'recurse))
        (when (eq? first-round-exit-status 'restart) (end-run 'restart))

        ; continue
        (when (eq? first-round-exit-status 'next-chapter) (next-chapter!)))

      
      (let loop ()
        (define round-exit-status (resolve-round 'begin))
        ; end run?
        (when (eq? round-exit-status 'pc-dead) (end-run 'pc-dead))
        (when (eq? round-exit-status 'win-game) (end-run 'win-game))
        (when (eq? round-exit-status 'end-run) (end-run 'end-run))
        (when (eq? round-exit-status 'recurse) (end-run 'recurse))
        (when (eq? round-exit-status 'restart) (end-run 'restart))

        ; continue
        (when (eq? round-exit-status 'next-chapter) (next-chapter!))
        
        (loop))))
  (on-end-run run-exit-status)
  run-exit-status)
