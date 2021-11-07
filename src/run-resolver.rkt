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
(define (narrate-begin-run)
  (next-chapter!)
  
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
  

  (p "Otava is again following the Broker's trail through the foggy woods. This time, she's better prepared, she knows what lies ahead. She gets to the fork."))


; engine / run-resolver
(define (on-begin-run)
  (current-run (add1 (current-run)))
  (current-round 0)
  (remove-flag 'ending-run-allowed)
  (move-pc-to-location! (find-place-by-id 'perimeter))
  (narrate-begin-run)
  )

(define (on-begin-recurse-run)
  (current-run (add1 (current-run)))
  #;(current-round 0)
  (remove-flag 'ending-run-allowed)
  (add-feature-to-location! (find-place-by-id 'martaanvuo-docks) 'stiltman)
  (move-pc-to-location! (find-place-by-id 'perimeter))
  (narrate-begin-recurse-run)
  )

(define (on-continue-run)
  '()
  )

(define (narrate-restart)
  (p
   (take-random
    (list
     "Otava wanders Martaanvuo the rest of her life without finding what she's looking for."
     "Death comes quickly to Otava."
     "A strange sense of detachment overcomes Otava, and she finds the Cache she was looking for, and in it, enough gold-198 to pay off her debt. Unbeknownst to her, she would soon experience it all over again."
     "Otava finds passage through the swamps and disappears to the other side of the unnamed mountain range, to begin a new life herding the reindeer."
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
    [else
     (p "narrate-end-run: unhandled exit status: " (symbol->string exit-status))])
  (wait-for-confirm))

; engine / run-resolver
(define (resolve-run mode)

  (case mode
    ['continue (on-continue-run)]
    ['begin (on-begin-run)]
    
    ['recurse (on-begin-recurse-run)])
  
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
