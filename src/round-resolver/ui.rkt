#lang racket

(provide (all-defined-out))

(require
  "../core/io.rkt"
  "../core/session.rkt"
  "../core/utils.rkt"

  "../quests/quest.rkt"

  "../state/state.rkt"
)


; This is sort-of "player AI" / player controller type of stuff, not just strictly ui.
; This is because at the time of writing this, cleaning up round-resolver.rkt is priority.

(define (get-quit-text)
  (define r (d 1 100))
  (cond
    ((= r 1)
     "Martaanvuo will always be there for you, as it always has.")
    ((< r 5)
     "")
    ((< r 20)
     "Martaanvuo is always there for you.")
    (else
     "Martaanvuo awaits your return.")
    ))

; TODO: clean up!
; Return value is tied to round resolution.
(define (quit)
  (displayln "Your attention is the thin barrier between the world and the void. Really quit?")
  (newline)
  (displayln "[Q] to quit, anything else to continue.")
  (define input (wait-for-input))
  (set! input (string-upcase input))
  (cond ((equal? input "Q")
         (define session-score (d (current-session-score-dice) 4))
         (prln (format "Session score: ~a" session-score))
         (newline)

         (define quit-message (get-quit-text))
         (when (not (equal? quit-message ""))
          (prln quit-message)
          (newline)
         )

         (exit))
        (else
         (newline)
         #t))) ; mark input as handled

(define (restart)
  (displayln "We live with the choices we make. Really restart?")
  (newline)
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