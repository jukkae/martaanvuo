#lang racket

(provide (all-defined-out))

(require "../pc.rkt"
         "../io.rkt"
         "../quest.rkt"
         "../situation.rkt"
         "../utils.rkt")

; This is sort-of "player AI" / player controller type of stuff, not just strictly ui.
; This is because at the time of writing this, cleaning up round-resolver.rkt is priority.
; TODO: clean up!
; Return value is tied to round resolution.

; engine / get-next-pc-action
(define (meta-command-valid? meta-commands-with-keys input)
  (set! input (string-upcase input))
  (define meta-command (hash-ref meta-commands-with-keys input '()))
  (if (not (null? meta-command))
      meta-command
      #f))

(define (quit)
  (displayln "Really quit Martaanvuo? [Q] to quit, anything else to continue.")
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
  (displayln
   (string-append
    "Really restart? [R] to restart, anything else to continue."))
  (define input (wait-for-input))
  (set! input (string-upcase input))
  (cond ((equal? input "R")
         'restart)
        (else
         (newline)
         #t))) ; mark input as handled


(define (menu)
  (define (handle-meta-command meta-commands-with-keys input)
    (set! input (string-upcase input))
    (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
    (define meta-command (cdr meta-command-with-key))
    (meta-command))
  (define (close-menu) #t) ; hacky but eh
  
  (displayln "[Menu]")
  (define meta-commands (make-hash))
  (hash-set! meta-commands "C" (cons "[C]: Close menu." close-menu))
  ;(hash-set! meta-commands "D" (cons "[D]: Delete progress." delete-progress))
  (hash-set! meta-commands "P" (cons "[P]: Player status." player-info))
  (hash-set! meta-commands "Q" (cons "[Q]: Quit Martaanvuo." quit))
  (hash-set! meta-commands "R" (cons "[R]: Restart." restart))
  

  (for ([(k v) (in-hash meta-commands)])
    (display (car v))
    (display " "))
  (newline)
  (newline)
  (define input (wait-for-input))
  (serialize-input)

  (newline)

  (cond ((meta-command-valid? meta-commands input) (handle-meta-command meta-commands input))
        (else (menu))))

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