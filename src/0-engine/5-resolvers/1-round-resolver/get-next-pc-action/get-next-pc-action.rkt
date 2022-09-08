#lang at-exp racket

(provide
 get-next-pc-action
 )

(require
  "menu.rkt"
  "pc-choices.rkt"

  "../fragment-handler.rkt"

  "../../2-action-resolver/action-resolver.rkt"

  "../../../2-core/io.rkt"
  "../../../2-core/core.rkt"

  "../../../3-types/action.rkt"
  "../../../3-types/choice.rkt"
  "../../../3-types/decision.rkt"
  "../../../3-types/item.rkt"
  "../../../3-types/location.rkt"
  "../../../3-types/place.rkt"
  "../../../3-types/pc-actor.rkt"
  "../../../3-types/world.rkt"

  "../../../4-systems/locations/locations.rkt"
  ;;; "../../../4-systems/pc/character-sheet.rkt"
  "../../../4-systems/pc/pc.rkt"
  "../../../4-systems/fragments.rkt"
  "../../../4-systems/world/time.rkt"
  "../../../4-systems/world/world.rkt"

  "../../../7-state/state.rkt"

  "../../../../1-content/narration/display-statusline.rkt"
  "../../../../1-content/narration/describe-situation.rkt"
  )

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
        (clear-current-fragment!)
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
                ;  (dev-note "fragment-decision-result: PC DEAD")
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

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (rules-to-lambda rules)
  `(λ () ,@rules))

(define (resolve-choice-and-produce-action! choices-with-keys input)
  (define resolution-effect (choice-as-resolution-effect choices-with-keys input))
  (define current-choice (hash-ref choices-with-keys (string->number input) '()))
  (define action
    (cond ((procedure? resolution-effect)
           (resolution-effect))
          [(action? resolution-effect)
           resolution-effect]
          [(null? resolution-effect)
           'end-round-early]
          [(sexp? resolution-effect)
           (define rules (rules-to-lambda resolution-effect))
           (define result ((eval rules ns)))
           (if (or (null? void?) result)
             'end-round-early
             result) ; TODO: result is an action result, specifically
           ]
          (else (error (format "resolve-choice-and-produce-action!: unknown resolution-effect: ~a." resolution-effect)))))

  ; TODO: uurgh
  (when (null? action)
    ; TODO: extract empty-action or something
    (set! action
      (make-action #:symbol 'empty-action #:actor (pc) #:duration 0)))

  (when (or (string-prefix? (choice-name current-choice) "[continue]")
            (string-prefix? (choice-name current-choice) "[cancel]"))
    (reset-pending-action!))

  action)

(define (choice-as-resolution-effect choices-with-keys input)
  (choice-resolution-effect (hash-ref choices-with-keys (string->number input) '())))

