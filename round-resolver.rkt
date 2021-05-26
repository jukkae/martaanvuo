#lang racket

(provide (all-defined-out))

(require "action.rkt")
(require "actions.rkt")
(require "actor.rkt")
(require "blindscraper.rkt")
(require "character-sheet.rkt")
(require "fragment.rkt")
(require "fragments.rkt")
(require "grabberkin.rkt")
(require "io.rkt")
(require "location.rkt")
(require "pc.rkt")
(require "situation.rkt")
(require "utils.rkt")
(require "world.rkt")

; fragment handler
(define (current-fragment-on-begin-round!)
  (paragraph (story-fragment-description (situation-current-fragment *situation*)))
  )

; fragment handler
(define (current-fragment-get-decisions)
  (filter (lambda (potential-decision)
            ((decision-requirement potential-decision)))
          (story-fragment-decisions (situation-current-fragment *situation*))))

; fragment handler
; move specifics from here to the actual fragment
(define (current-fragment-handle-decision! decision)

  (paragraph (decision-description decision))
  (define next-fragment (decision-next-fragment decision))

  ; brilliant idea x dirty hack
  (when (procedure? next-fragment)
    (set! next-fragment (next-fragment)))
  (cond ((number? next-fragment)
         (go-to-story-fragment next-fragment)
         )
        ((eq? 'exit next-fragment)
         (set-situation-current-fragment! *situation* '()))
        ((eq? 'exit-and-set-build-desperate next-fragment)
         (set-build! 'desperate)
         (set-situation-current-fragment! *situation* '()))
        ((eq? 'exit-and-set-build-bruiser next-fragment)
         (set-build! 'bruiser)
         (set-situation-current-fragment! *situation* '()))
        (else (error (string-append "(current-fragment-handle-decision!): next-fragment type not implemented: " (symbol->string next-fragment)))))
  )

; fragment handler
(define (current-fragment-on-end-round!)
  '()
  )

; fragment handler
(define (go-to-story-fragment id)
  (set-situation-current-fragment! *situation* (get-fragment id))
  ((story-fragment-on-enter! (situation-current-fragment *situation*))))

; fragment handler
(define (handle-fragment-decision decisions-with-keys input)
  (define decision (hash-ref decisions-with-keys (string->number input)))
  (current-fragment-handle-decision! decision))

; engine / round resolver: ai dispatching
(define (get-next-npc-action actor)
  (case (actor-name actor)
    (["Blindscraper"] (get-blindscraper-action actor))
    (["Grabberkin"] (get-grabberkin-action actor))
    (else (displayln "get-next-npc-action: unknown actor"))))



; engine / round resolver: implementation detail
(define action-queue '())
; engine / round resolver
(define (add-to-action-queue action)
  (set! action-queue (cons action action-queue)))
; engine / round resolver
(define (remove-from-action-queue actions)
  (set! action-queue (remq* actions action-queue)))
; engine / round resolver
(define (sort-action-queue)
  
  (define actions-by-initiatives '())
  (for ([action action-queue])
    (define actor (action-actor action))
    (define dexterity-mod (get-attribute-modifier-for (actor-dexterity actor)))
    
    (define action-mod 0)
    
    (cond ((has-tag? action 'fast)
           (set! action-mod 2))
          ((has-tag? action 'slow)
           (set! action-mod -4)))



    (define dice-1 (d 1 6))
    (define dice-2 (d 1 6))

    (define total (+ dice-1 dice-2 action-mod dexterity-mod))


    (set! actions-by-initiatives (append-element actions-by-initiatives (cons total action))))

  (define shuffled (shuffle actions-by-initiatives)) ; shuffle to avoid sort stability
  (define sorted (sort shuffled
                       (λ (a1 a2) (> (car a1) (car a2))))) ; intentionally flipped: Higher is better

  (define actions
    (for/list ([action-with-initiative sorted])
      (define action (cdr action-with-initiative))
      (define initiative (car action-with-initiative))
      (define action-description
        (string-append
         " "
         (actor-name (action-actor action))
         #;": "
         #;(symbol->string (action-symbol action))
         " "))
      (list action-description (string-append " " (number->string initiative) " "))))
  
  (info-card actions "Action initiatives")

  (set! action-queue '())
  (for ([action-with-initiative sorted])
    (set! action-queue (append-element action-queue (cdr action-with-initiative))))
  
  action-queue)


; engine / round resolver
(define (on-begin-round)
  (set-situation-round! *situation* (add1 (situation-round *situation*)))
  (define round-summary
    (list
     (list " round "
           (string-append
            " "
            (number->string (situation-round *situation*))
            " "))
     (list " current location "
           (string-append
            " "
            (symbol->string (location-type (current-location)))
            " "))
     (list " time of day " (string-append " " (symbol->string (time-of-day-from-jiffies (world-elapsed-time (situation-world *situation*)))) " "))
     (list " elapsed time (total) " (string-append " " (number->string (world-elapsed-time (situation-world *situation*))) " "))
     ))
  (info-card round-summary (string-append "Begin round " (number->string (situation-round *situation*))))
  
  (set! action-queue '())
  
  (when (not (null? (situation-current-fragment *situation*)))
    (current-fragment-on-begin-round!))
  )

; engine / round resolver
(define (get-next-action actor)
  (cond ((not (pc-actor? actor)) (get-next-npc-action actor))
        (else
         (serialize-state)
         (get-next-pc-action)))
  )


; engine / round resolver
(define (enqueue-npc-actions)
  (define actors (location-actors (current-location)))
  (for ([actor actors])
    (when (not (pc-actor? actor))
      (define next-action (get-next-action actor))
      (add-to-action-queue next-action))))


; engine / get-next-pc-action
(define (get-next-pc-action)
  (serialize-state)
  (let/ec produce-action
    (let what-do-you-do ([verbosity 'verbose])
      (define (handle-meta-command meta-commands-with-keys input)
        (set! input (string-upcase input))
        (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
        (define meta-command (cdr meta-command-with-key))
        (meta-command)
        (redescribe-situation)
        (what-do-you-do 'verbose))
      
      (define actor (situation-pc *situation*))


      (define fragment-decisions (if (null? (situation-current-fragment *situation*))
                                     '()
                                     (current-fragment-get-decisions)))
      (define world-choices (get-world-choices (situation-world *situation*) actor))
      
      (define choices (if (null? fragment-decisions)
                          world-choices
                          '()))

      (define fragment-decisions-with-keys (build-keys-to-choices-map fragment-decisions 1))
      (define first-non-fragment-index (add1 (length fragment-decisions)))
      (define choices-with-keys (build-keys-to-choices-map choices first-non-fragment-index)) ; should check for pending actions and name choices accordingly
      (define meta-commands-with-keys (get-meta-commands-with-keys))
      
      (print-choices-and-meta-commands-with-keys choices-with-keys fragment-decisions-with-keys meta-commands-with-keys verbosity)
      (define input (wait-for-input))
      (serialize-input)

      (newline)

      (cond ((meta-command-valid? meta-commands-with-keys input) (handle-meta-command meta-commands-with-keys input))
            ((fragment-decision-valid? fragment-decisions-with-keys input)
             (begin
               (handle-fragment-decision fragment-decisions-with-keys input)
               produce-action 'end-round-early))
            ((choice-valid? choices-with-keys input) (produce-action (choice-as-action choices-with-keys input)))
            (else (what-do-you-do 'abbreviated))))))


; engine / get-next-pc-action
(define (meta-command-valid? meta-commands-with-keys input)
  (set! input (string-upcase input))
  (define meta-command (hash-ref meta-commands-with-keys input '()))
  (if (not (null? meta-command))
      meta-command
      #f))

; engine / get-next-pc-action
(define (choice-valid? choices-with-keys input)
  (define choice (hash-ref choices-with-keys (string->number input) '()))
  (if (not (null? choice))
      choice
      #f))

; engine / get-next-pc-action
(define (fragment-decision-valid? decisions-with-keys input)
  (define decision (hash-ref decisions-with-keys (string->number input) '()))
  (if (not (null? decision))
      decision
      #f))

; engine / get-next-pc-action
(define (choice-as-action choices-with-keys input)
  ((choice-resolution-effect (hash-ref choices-with-keys (string->number input) '()))))

; engine / get-next-pc-action
(define (print-choices-with-keys choices-with-keys)
  ; TODO: Should order here based on key
  (for ([(k v) (in-hash choices-with-keys)])
    (displayln (string-append "[" (number->string k) "]: " (choice-name v))))
  (newline))

; engine / get-next-pc-action
(define (print-decisions-with-keys decisions-with-keys)
  (for ([(k v) (in-hash decisions-with-keys)])
    (displayln (string-append "[" (number->string k) "]: " (decision-title v))))
  #;(newline))
  
; engine / get-next-pc-action
(define (key-from-index i)
  (cond ((< i 0) (error "negative index!"))
        ((<= i 8) (add1 i))
        ((= i 9) 0)
        ((> i 9) (error "too many things to do!"))))

; engine / get-next-pc-action
(define (build-keys-to-choices-map choices first-index)
  (define choices-with-keys (make-hash))
  (for ([i (in-range (length choices))])
    (define key (key-from-index (+ first-index i -1)))
    (hash-set! choices-with-keys key (list-ref choices i)))
  choices-with-keys)

; engine / get-next-pc-action
(define (get-meta-commands-with-keys)
  (define meta-commands (make-hash))
  #;(hash-set! meta-commands "D" (cons "[D]: Describe situation again." describe-situation))
  (hash-set! meta-commands "M" (cons "[M]: Menu." menu))
  (hash-set! meta-commands "C" (cons "[C]: Character sheet." character-sheet))
  (hash-set! meta-commands "I" (cons "[I]: Inventory." inventory))
  (hash-set! meta-commands "Q" (cons "[Q]: Quests." quests))
  meta-commands)

; engine / get-next-pc-action
(define (print-meta-commands-with-keys meta-commands-with-keys)
  (for ([(k v) (in-hash meta-commands-with-keys)])
    (display (car v))
    (display " "))
  (newline)
  (newline))

; engine / get-next-pc-action
(define (print-choices-and-meta-commands-with-keys choices-with-keys fragment-decisions-with-keys meta-commands-with-keys verbosity)
  (cond ((eq? verbosity 'abbreviated)
         (display "Unknown command. Known commands: ")
         (for ([(k v) (in-hash fragment-decisions-with-keys)]) (display k))
         (for ([(k v) (in-hash choices-with-keys)]) (display k))
         (for ([(k v) (in-hash meta-commands-with-keys)]) (display k))
         (newline)
         )
        (else
         (newline) ; This is extra spacing, should pass a param to paragraph
         #;(paragraph "What do you do?")
         (print-decisions-with-keys fragment-decisions-with-keys)
         (print-choices-with-keys choices-with-keys)
         (print-meta-commands-with-keys meta-commands-with-keys))))




(displayln "todo find me fix me")
; UI? meta? scripting api? return value tied to round resolution
(define (quit)
  (displayln "Really quit? [Q] to quit, anything else to continue.")
  (define input (wait-for-input))
  (set! input (string-upcase input))
  (cond ((equal? input "Q")
         (paragraph "Game exited.")
         (exit))
        (else
         (newline)
         #t))) ; mark input as handled

; UI? meta? scripting api? return value tied to round resolution
(define (menu)
  (define (handle-meta-command meta-commands-with-keys input)
    (set! input (string-upcase input))
    (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
    (define meta-command (cdr meta-command-with-key))
    (meta-command))
  (define (close-menu) #t) ; hacky but eh
  
  (displayln "[Menu]")
  (define meta-commands (make-hash))
  (hash-set! meta-commands "Q" (cons "[Q]: Quit Martaanvuo." quit))
  (hash-set! meta-commands "C" (cons "[C]: Close menu." close-menu))

  (for ([(k v) (in-hash meta-commands)])
    (display (car v))
    (display " "))
  (newline)
  (newline)
  (define input (wait-for-input))
  (serialize-input)

  (newline)

  (cond ((meta-command-valid? meta-commands input) (handle-meta-command meta-commands input))
        (else (menu)))
  #t)

; pc? meta? api?
(define (inventory)
  (define actor (situation-pc *situation*))
  
  (define sheet
    (append
     (list
      (list " Item " " Notes "))
     (actor-inventory actor)))
  (info-card
   sheet
   "Inventory"
   )
  #t
  )