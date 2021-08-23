#lang racket

(provide build-keys-to-choices-map
         choice-valid?
         fragment-decision-valid?
         get-meta-commands-with-keys
         get-next-pc-action
         meta-command-valid?
         print-choices-and-meta-commands-with-keys
         print-meta-commands-with-keys
         resolve-choice-and-produce-action!)

(require racket/lazy-require)

(require "../action.rkt"
         "../actions.rkt"
         "../actor.rkt"
         "../character-sheet.rkt"
         "../choice.rkt"
         "../decision.rkt"
         "../io.rkt"
         "../locations.rkt"
         "../situation.rkt")

(require "fragment-handler.rkt")

(lazy-require
 ["../situation.rkt"
  (player-info
   )])

(lazy-require
 ["ui.rkt"
  (display-quests
   inventory
   notes
   quit
   restart
   )])


; From an "outside" perspective, this should be called "handle-meta-or-get-next-pc-action", or something like that –
; this pokes a hole through abstraction layers (as it should)
; (sort of like IO monad)
(define (get-next-pc-action)
  (let/ec produce-action
    (let what-do-you-do ([verbosity 'verbose])
      (define (handle-meta-command meta-commands-with-keys input)
        (set! input (string-upcase input))
        (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
        (define meta-command (cdr meta-command-with-key))
        (define meta-command-result (meta-command))
        (when (eq? meta-command-result 'restart) (produce-action 'restart))
        
        
        (redescribe-situation)
        (what-do-you-do 'verbose))
      
      (define actor (situation-pc *situation*))


      (define fragment-decisions (if (null? (situation-current-fragment-id *situation*))
                                     '()
                                     (current-fragment-get-decisions)))

      (when (null? fragment-decisions)
        (wait-for-confirm)) ; what a place for this

      ; launch a fragment directly -> no action resolution -> not a choice
      (define location-decisions (if (null? (situation-current-fragment-id *situation*))
                                     (get-location-decisions (current-location))
                                     '()))
      
      (define world-choices (get-world-choices (situation-world *situation*) actor))
      
      (define choices (if (null? fragment-decisions)
                          world-choices
                          '()))

      (define all-decisions (append fragment-decisions location-decisions))
      (define decisions-with-keys (build-keys-to-choices-map all-decisions 1))
      (define first-free-index (add1 (length all-decisions)))
      (define choices-with-keys (build-keys-to-choices-map choices first-free-index)) ; should check for pending actions and name choices accordingly
      (define meta-commands-with-keys (get-meta-commands-with-keys))
      
      (when (not (eq? "" (get-prompt)))
        (display-prompt))

      (print-choices-and-meta-commands-with-keys choices-with-keys decisions-with-keys meta-commands-with-keys verbosity)

      (define input (wait-for-input))

      (newline)

      (cond ((meta-command-valid? meta-commands-with-keys input) (handle-meta-command meta-commands-with-keys input))
            ((fragment-decision-valid? decisions-with-keys input)
             (begin
               (define fragment-decision-result (handle-fragment-decision decisions-with-keys input))
               
               (define result 'end-round-early)
               (when (eq? fragment-decision-result 'recurse)
                 (set! result 'recurse))
               produce-action result))
            ((choice-valid? choices-with-keys input) (produce-action (resolve-choice-and-produce-action! choices-with-keys input)))
            (else (what-do-you-do 'abbreviated))))))


(define (choice-valid? choices-with-keys input)
  (define choice (hash-ref choices-with-keys (string->number input) '()))
  (if (not (null? choice))
      choice
      #f))

(define (fragment-decision-valid? decisions-with-keys input)
  (define decision (hash-ref decisions-with-keys (string->number input) '()))
  (if (not (null? decision))
      decision
      #f))

(define (resolve-choice-and-produce-action! choices-with-keys input)
  (define resolution-effect (choice-as-resolution-effect choices-with-keys input))

  (define action
    (cond ((procedure? resolution-effect) (resolution-effect))
          ((action? resolution-effect) resolution-effect)
          (else (error "resolve-choice-and-produce-action!: unknown type"))))

  ; dirty to do this here like this but eh
  (define pending-choice-available? #f)
  (for/hash ([(k v) (in-hash choices-with-keys)])
    (values k
            (begin
              (when (string-prefix? (choice-name v) "[continue]")
                (set! pending-choice-available? #t)))))
  
  ; choice either is pending (= resolve it) or is not, in which case discard pending action
  (when pending-choice-available? (reset-pending-action!))
  
  action)

(define (choice-as-resolution-effect choices-with-keys input)
  (choice-resolution-effect (hash-ref choices-with-keys (string->number input) '())))

(define (key-from-index i)
  (cond ((< i 0) (error "negative index!"))
        ((<= i 8) (add1 i))
        ((= i 9) 0)
        ((> i 9) (error "too many things to do!"))))

(define (build-keys-to-choices-map choices first-index)
  (define choices-with-keys (make-hash))
  (for ([i (in-range (length choices))])
    (define key (key-from-index (+ first-index i -1)))
    (hash-set! choices-with-keys key (list-ref choices i)))
  choices-with-keys)

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

  (newline)

  (cond ((meta-command-valid? meta-commands input) (handle-meta-command meta-commands input))
        (else (menu))))

; THIS IS THE BASIC META MENU
(define (get-meta-commands-with-keys)
  (define meta-commands (make-hash))
  (hash-set! meta-commands "M" (cons "[M]: Menu." menu))
  (hash-set! meta-commands "N" (cons "[N]: Notes." notes))
  (hash-set! meta-commands "C" (cons "[C]: Character sheet." character-sheet))
  (when (not (null? (actor-inventory (pc))))
    (hash-set! meta-commands "I" (cons "[I]: Inventory." inventory)))
  (hash-set! meta-commands "L" (cons "[L]: Logs." display-log))
  (hash-set! meta-commands "Q" (cons "[Q]: Quests." display-quests))
  meta-commands)

(define (print-decisions-with-keys decisions-with-keys)
  (for ([(k v) (in-hash decisions-with-keys)])
    (displayln (string-append "[" (number->string k) "]: " (decision-title v))))
  #;(newline))

(define (print-choices-with-keys choices-with-keys)
  (define choices
    (for/list ([(k v) (in-hash choices-with-keys)])
      (cons k v)))
  
  (set! choices
        (sort choices
              (λ (c1 c2) (< (car c1) (car c2)))))
  
  (for ([choice choices])
    (displayln (string-append "[" (number->string (car choice)) "]: " (choice-name (cdr choice)))))
  (newline))

(define (print-meta-commands-with-keys meta-commands-with-keys)
  (for ([(k v) (in-hash meta-commands-with-keys)])
    (display (car v))
    (display " "))
  (newline)
  (newline))

(define (print-choices-and-meta-commands-with-keys
         choices-with-keys
         fragment-decisions-with-keys
         meta-commands-with-keys verbosity)
  (cond ((eq? verbosity 'abbreviated)
         (display "Unknown command. Known commands: ")
         (for ([(k v) (in-hash fragment-decisions-with-keys)]) (display k))
         (for ([(k v) (in-hash choices-with-keys)]) (display k))
         (for ([(k v) (in-hash meta-commands-with-keys)]) (display k))
         (newline)
         )
        (else
         (newline) ; This is the extra space, should pass a param to print call perhaps instead?
         #;(p "What do you do?")
         (print-decisions-with-keys fragment-decisions-with-keys)
         (print-choices-with-keys choices-with-keys)
         (print-meta-commands-with-keys meta-commands-with-keys))))

(define (meta-command-valid? meta-commands-with-keys input)
  (set! input (string-upcase input))
  (define meta-command (hash-ref meta-commands-with-keys input '()))
  (if (not (null? meta-command))
      meta-command
      #f))