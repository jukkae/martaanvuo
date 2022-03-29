#lang at-exp racket

(provide
 get-next-pc-action
 meta-command-valid?
 print-meta-commands-with-keys
 )

(require racket/lazy-require)

(require
  "fragment-handler.rkt"

  "../../2-core/io.rkt"
  "../../2-core/core.rkt"

  "../../3-types/pc-actor.rkt"
  "../../3-types/item.rkt"
  "../../3-types/location.rkt"
  "../../3-types/world.rkt"
  "../../3-types/action.rkt"

  "../../4-rules/pc-choices/pc-choices.rkt"
  "../../4-rules/pc-choices/choice.rkt"
  "../../4-rules/fragments/decision.rkt"
  "../../4-rules/locations/locations.rkt"
  "../../4-rules/pc/character-sheet.rkt"
  "../../4-rules/pc/pc.rkt"
  "../../4-rules/world/time.rkt"

  "../../7-state/state/state.rkt"

  "../../../1-content/narration/display-statusline.rkt"
  "../../../1-content/narration/describe-situation.rkt"
  )

(lazy-require ["ui.rkt"
  (display-tasks
   display-log
   inventory
   quit
   restart
   )])


; From an "outside" perspective, this should be called "handle-meta-or-get-next-pc-action", or something like that –
; this pokes a hole through abstraction layers (as it should)
; (sort of like IO monad)
; Returns action or symbol
(define (get-next-pc-action)
  (let/ec return
    (let what-do-you-do ([question-repeated? #f])
      (define (handle-meta-command meta-commands-with-keys input)
        (set! input (string-upcase input))
        (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
        (define meta-command (cdr meta-command-with-key))
        (define meta-command-result (meta-command))
        (when (eq? meta-command-result 'restart) (return 'restart))

        (what-do-you-do #t))

      (define actor (pc))

      (define fragment-decisions (if (null? (current-fragment-id))
                                     '()
                                     (current-fragment-get-decisions)))

      (when (null? fragment-decisions)
        (unset-current-fragment-id!)
        )

      ; launch a fragment directly -> no action resolution -> not a choice
      (define location-decisions (if (null? fragment-decisions)
                                     (get-location-decisions (current-location))
                                     '()))

      (define world-choices (get-world-choices (current-world) actor))

      (define choices (if (null? fragment-decisions)
                          world-choices
                          '()))

      (define all-decisions (append fragment-decisions location-decisions))
      (define decisions-with-keys (build-keys-to-choices-map all-decisions 1))
      (define first-free-index (add1 (length all-decisions)))
      (define choices-with-keys (build-keys-to-choices-map choices first-free-index)) ; should check for pending actions and name choices accordingly
      (define meta-commands-with-keys (get-meta-commands-with-keys))

      (if question-repeated?
          (describe-situation #t)
          (describe-situation #f)
          )

      (newline)
      (display-statusline)
      (newline)

      (when (not (eq? "" (current-prompt)))
        (display-prompt))

      (print-choices-and-meta-commands-with-keys choices-with-keys decisions-with-keys meta-commands-with-keys)

      (define input (wait-for-input))

      (newline)

      (cond ((meta-command-valid? meta-commands-with-keys input)
             (handle-meta-command meta-commands-with-keys input))

            ((fragment-decision-valid? decisions-with-keys input)
             (begin
               (define fragment-decision-result (handle-fragment-decision decisions-with-keys input))

               (define result 'end-round-early)
               (when (eq? fragment-decision-result 'recurse)
                 (set! result 'recurse))
               (when (eq? fragment-decision-result 'pc-dead)
                 (dev-note "fragment-decision-result: PC DEAD")
                 (set! result 'pc-dead))
               return result))

            ((choice-valid? choices-with-keys input)
             (define action (resolve-choice-and-produce-action! choices-with-keys input))
             (cond ((eq? 'cancel action) (what-do-you-do #f))
                   (else (return action))))

            (else
             (notice
              ; if whitespace only
              (if (regexp-match-exact? #px"\\s*" input)
                  "Empty command"
                  (format "Unknown command: [~a]" input)
                  )
              )
             (newline)
             (what-do-you-do #t))))))


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
  (define current-choice (hash-ref choices-with-keys (string->number input) '()))

  (define action
    (cond ((procedure? resolution-effect) (resolution-effect))
          ((action? resolution-effect) resolution-effect)
          (else (error "resolve-choice-and-produce-action!: unknown type"))))

  (when (or (string-prefix? (choice-name current-choice) "[continue]")
            (string-prefix? (choice-name current-choice) "[cancel]"))
    (reset-pending-action!))

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
  (hash-set! meta-commands "C" (cons "[C]: Character sheet." character-sheet))
  #;(when (not (null? (actor-inventory (pc))))
      (hash-set! meta-commands "I" (cons "[I]: Inventory." inventory)))
  (hash-set! meta-commands "L" (cons "[L]: Logs." display-log))
  (hash-set! meta-commands "T" (cons "[T]: Tasks." display-tasks))
  meta-commands)

(define (print-decisions-with-keys decisions-with-keys)
  (for ([(k v) (in-hash decisions-with-keys)])
    (displayln (format "[~a]: ~a" k (decision-title v))))
  #;(newline))

(define (print-choices-with-keys choices-with-keys)
  (define choices
    (for/list ([(k v) (in-hash choices-with-keys)])
      (cons k v)))

  (set! choices
        (sort choices
              (λ (c1 c2) (< (car c1) (car c2)))))

  (for ([choice choices])
    (displayln (format "[~a]: ~a" (car choice) (choice-name (cdr choice)))))
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
         meta-commands-with-keys)
  (print-decisions-with-keys fragment-decisions-with-keys)
  (print-choices-with-keys choices-with-keys)
  (print-meta-commands-with-keys meta-commands-with-keys))

(define (meta-command-valid? meta-commands-with-keys input)
  (set! input (string-upcase input))
  (define meta-command (hash-ref meta-commands-with-keys input '()))
  (if (not (null? meta-command))
      meta-command
      #f))
