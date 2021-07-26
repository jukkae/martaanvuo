#lang racket

(provide (all-defined-out))

(require "io.rkt")
(require "round-resolver.rkt")
(require "situation.rkt")
(require "world.rkt")


; content should be provided "somewhere"
; content is game-specific, not engine stuff
; but figure out where this should be called from
; engine / run-resolver
(define (narrate-begin-run)
  ; Don't show this until the second run!
  (when (not (= 1 (situation-run *situation*)))
    (info-card
     (list
      (list " run " (string-append " " (number->string (situation-run *situation*)) " ")))
     (string-append "Begin run number " (number->string (situation-run *situation*)))))

  (next-chapter!)
  (case (situation-run *situation*)
    [(1)
     (paragraph "Otava is following an old, overgrown trail through foggy woods. The air is thick with a damp, musty smell.")
     (when (not (quest-exists? 'pay-off-debt))
       (create-quest 'pay-off-debt))]
    [(2)
     (paragraph "\"How many more,\" she thinks as she goes down the path toward Perimeter, \"can I do?\"")]))


; engine / run-resolver
(define (on-begin-run)
  (set-situation-run! *situation* (add1 (situation-run *situation*)))
  (set-situation-round! *situation* 0)
  (move-actor-to-location! (situation-pc *situation*) perimeter)
  (narrate-begin-run)
  )

(define (narrate-end-run exit-status)
  (info-card
   (list
    (list " run " (string-append " " (number->string (situation-run *situation*)) " ")))
   (string-append "End run number " (number->string (situation-run *situation*))))
  (case exit-status
    ['end-run
     (paragraph "She's still alive.")]
    [else
     (paragraph "narrate-end-run: unhandled exit status: " (symbol->string exit-status))]))

(define (on-end-run exit-status)
  (narrate-end-run exit-status)
  (wait-for-confirm))

; engine / run-resolver
(define (resolve-a-run)
  (on-begin-run)
  (define run-exit-status
    (let/ec end-run
      (let loop ()
        (define round-exit-status (resolve-round))
        ; end run?
        (when (eq? round-exit-status 'pc-dead) (end-run 'pc-dead))
        (when (eq? round-exit-status 'win-game) (end-run 'win-game))
        (when (eq? round-exit-status 'end-run) (end-run 'end-run))

        ; continue
        (when (eq? round-exit-status 'next-chapter) (next-chapter!))
        (loop))))
  (on-end-run run-exit-status)
  run-exit-status)

(define (continue-run)
  (define run-exit-status
    (let/ec end-run
      (let loop ()
        (define round-exit-status (resolve-round))
        ; end run?
        (when (eq? round-exit-status 'pc-dead) (end-run 'pc-dead))
        (when (eq? round-exit-status 'win-game) (end-run 'win-game))
        (when (eq? round-exit-status 'end-run) (end-run 'end-run))

        ; continue
        (when (eq? round-exit-status 'next-chapter) (next-chapter!))
        (loop))))
  (on-end-run run-exit-status)
  run-exit-status
  )