#lang racket

(provide (all-defined-out))

(require "../character-sheet.rkt"
         "../io.rkt"
         "../pc.rkt"
         "../quest.rkt"
         "../utils.rkt"

         "../state/state.rkt")

(require "get-next-pc-action.rkt")

; This is sort-of "player AI" / player controller type of stuff, not just strictly ui.
; This is because at the time of writing this, cleaning up round-resolver.rkt is priority.
; TODO: clean up!
; Return value is tied to round resolution.


(define (quit)
  (displayln "Your attention is the thin barrier between everything and the void. Really quit?")
  (displayln "[Q] to quit, anything else to continue.")
  (define input (wait-for-input))
  (set! input (string-upcase input))
  (cond ((equal? input "Q")
         (define session-score (d 1 4))
         (p (string-append "Your session score was " (number->string session-score) "."))
         (p "Martaanvuo expects your return.")
         (exit))
        (else
         (newline)
         #t))) ; mark input as handled

(define (restart)
  (displayln "We have to live with the choices we make. Really restart?")
  (displayln "[R] to restart, anything else to continue.")
  (define input (wait-for-input))
  (set! input (string-upcase input))
  (cond ((equal? input "R")
         'restart)
        (else
         (newline)
         #t))) ; mark input as handled




(define (inventory)
  (inventory)
  #t
  )


(define (notes)
  (define actor (pc))
  
  (define list-items
    (list
     (list " Martaanvuo " " The anomaly is very strong here. ")))

  
  (info-card
   list-items
   "Notes"
   )
  #t
  )

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