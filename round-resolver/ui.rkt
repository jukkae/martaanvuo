#lang racket

(provide (all-defined-out))

(require "../pc.rkt"
         "../io.rkt"
         "../quest.rkt"
         "../situation.rkt")

; This is sort-of "player AI" / player controller type of stuff, not just strictly ui.
; This is because at the time of writing this, cleaning up round-resolver.rkt is priority.
; TODO: clean up!
; Return value is tied to round resolution.

(define (inventory)
  (print-inventory)
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