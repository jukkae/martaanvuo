#lang racket

(provide (all-defined-out))

(require "io.rkt")
(require "pc.rkt")
(require "quest.rkt")
(require "quests.rkt")
(require "round-resolver.rkt")
(require "situation.rkt")
(require "utils.rkt")
(require "world.rkt")


; content should be provided "somewhere"
; content is game-specific, not engine stuff
; but figure out where this should be called from
; engine / run-resolver
(define (narrate-begin-run)
  (next-chapter!)
  
  ; Don't show this until the second run!
  (when (not (= 1 (situation-run *situation*)))
    (info-card
     (list
      (list " run " (string-append " " (number->string (situation-run *situation*)) " ")))
     (string-append "Begin run number " (number->string (situation-run *situation*)))))

  
  (case (situation-run *situation*)
    [(1)
     (paragraph "Otava is following an old, overgrown trail through foggy woods. The air is thick with a damp, musty smell. The Broker's instructions have been correct thus far. Somebody has been here, once.")
     (when (not (quest-exists? 'pay-off-debt))
       (create-quest 'pay-off-debt))]
    #;[(2)
       (paragraph "\"How many more,\" she thinks as she goes down the path toward Perimeter, \"can I do?\"")]))


; engine / run-resolver
(define (on-begin-run)
  (set-situation-run! *situation* (add1 (situation-run *situation*)))
  (set-situation-round! *situation* 0)
  (remove-flag 'ending-run-allowed)
  (move-pc-to-location! (find-place-by-id 'perimeter))
  (narrate-begin-run)
  )

(define (on-continue-run)
  '()
  )

(define (narrate-restart)
  (paragraph
   (take-random
    (list
     "Otava wanders Martaanvuo the rest of her life without finding what she's looking for."
     "Death comes quickly to Otava."
     "A strange sense of detachment overcomes Otava, and she finds the Cache she was looking for, and in it, enough gold-198 to pay off her debt. Unbeknownst to her, she would soon experience it all over again."
     "Otava finds passage through the swamps and disappears to the other side of the unnamed mountain range, to begin a new life herding the reindeer."
     ))))


(define (on-end-run exit-status)
  (when (not (eq? exit-status 'restart))
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
                   (string-append " " (number->string (situation-run *situation*)) " "))
             (list " gold collected "
                   (string-append " " (number->string (pc-gold-amount)) " grams "))
             (list " debt still owed "
                   (string-append " " (number->string (quest-details debt-quest)) " grams ")))
            (string-append "Run number " (number->string (situation-run *situation*)) " ended")))

        
          (else
           (displayln "FAILED RUN")
           (info-card
            (list
             (list " run " (string-append " " (number->string (situation-run *situation*)) " ")))
            (string-append "End run number " (number->string (situation-run *situation*)))))))

  
  
  (case exit-status
    ['end-run
     (paragraph "She's still alive.")]
    ['restart
     (narrate-restart)]
    [else
     (paragraph "narrate-end-run: unhandled exit status: " (symbol->string exit-status))])
  (wait-for-confirm))

; engine / run-resolver
(define (resolve-run mode)
  (if (eq? mode 'continue)
      (on-continue-run)
      (on-begin-run))
  
  (define run-exit-status
    (let/ec end-run

      
      (when (eq? mode 'continue)
        (define first-round-exit-status (resolve-round 'continue))
        ; end run?
        (when (eq? first-round-exit-status 'pc-dead) (end-run 'pc-dead))
        (when (eq? first-round-exit-status 'win-game) (end-run 'win-game))
        (when (eq? first-round-exit-status 'end-run) (end-run 'end-run))
        (when (eq? first-round-exit-status 'restart) (end-run 'restart))

        ; continue
        (when (eq? first-round-exit-status 'next-chapter) (next-chapter!)))

      
      (let loop ()
        (define round-exit-status (resolve-round 'begin))
        ; end run?
        (when (eq? round-exit-status 'pc-dead) (end-run 'pc-dead))
        (when (eq? round-exit-status 'win-game) (end-run 'win-game))
        (when (eq? round-exit-status 'end-run) (end-run 'end-run))
        (when (eq? round-exit-status 'restart) (end-run 'restart))

        ; continue
        (when (eq? round-exit-status 'next-chapter) (next-chapter!))
        
        (loop))))
  (on-end-run run-exit-status)
  run-exit-status)
