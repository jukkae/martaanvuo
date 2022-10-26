#lang at-exp racket

(provide build-keys-to-choices-map
         get-meta-commands-with-keys
         meta-command-valid?
         print-choices-and-meta-commands-with-keys
         print-meta-commands-with-keys)

(require racket/lazy-require)

(require "pc-choices.rkt"

         "../fragment-handler.rkt"

         "../../../2-core/io.rkt"
         "../../../2-core/core.rkt"

         "../../../3-types/action.rkt"
         "../../../3-types/choice.rkt"
         "../../../3-types/decision.rkt"
         "../../../3-types/item.rkt"
         "../../../3-types/location.rkt"
         "../../../3-types/pc-actor.rkt"
         "../../../3-types/place.rkt"
         "../../../3-types/world.rkt"

         "../../../4-systems/locations/locations.rkt"
         "../../../4-systems/pc/pc.rkt"
         "../../../4-systems/world/time.rkt"

         "../../../7-state/state.rkt"

         "../../../../1-content/character-sheet/character-sheet.rkt"
         "../../../../1-content/narration/display-statusline.rkt")

(lazy-require ["ui.rkt" (display-tasks display-log inventory quit restart)])

(define (build-keys-to-choices-map choices first-index)
  (define choices-with-keys (make-hash))

  (for ([c choices] [i (in-naturals)])
    (hash-set! choices-with-keys (+ first-index i) (list-ref choices i)))

  choices-with-keys)

(define (menu)
  (define (handle-meta-command meta-commands-with-keys input)
    (set! input (string-upcase input))
    (define meta-command-with-key (hash-ref meta-commands-with-keys input '()))
    (define meta-command (cdr meta-command-with-key))
    (meta-command))
  (define (close-menu)
    #t) ; hacky but eh

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

  (cond
    [(meta-command-valid? meta-commands input) (handle-meta-command meta-commands input)]
    [else (menu)]))

; THIS IS THE BASIC META MENU
(define (get-meta-commands-with-keys)
  (define meta-commands (make-hash))
  (hash-set! meta-commands "M" (cons "[M]: Menu." menu))
  (hash-set! meta-commands
             "0"
             (cons "[0]: Character sheet." character-sheet)) ; TODO: [additional actions] notice here
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

  (set! choices (sort choices (λ (c1 c2) (< (car c1) (car c2)))))

  (for ([choice choices])
    (if (choice-unavailable? (cdr choice))
        (displayln (format "[ ]: ~a" (choice-name (cdr choice))))
        (displayln (format "[~a]: ~a" (car choice) (choice-name (cdr choice))))))
  (newline))

(define (print-static-choices-with-keys static-choices-with-keys)
  (for ([(k v) (in-hash static-choices-with-keys)])
    (define c (hash-ref static-choices-with-keys k '()))

    (cond [(and (not (null? c))
                (not (choice-unavailable? (cdr c))))
           (display (car v))]
          [else
           (display "[ ]")])
    (cond
      [(equal? k "0")
       (displayln "")
       (displayln "")]
      [else (display " ")]))
  (newline)
  (newline)
  ; (define choices
  ;   (for/list ([(k v) (in-hash static-choices-with-keys)])
  ;     (cons k v)))

  ; (set! choices (sort choices (λ (c1 c2) (< (car c1) (car c2)))))

  ; (for ([choice choices])
  ;   (if (choice-unavailable? (cdr choice))
  ;       (displayln (format "[ ]: ~a" (choice-name (cdr choice))))
  ;       (displayln (format "[~a]: ~a" (car choice) (choice-name (cdr choice))))))
  ; (newline)
  )

(define (print-meta-commands-with-keys meta-commands-with-keys)
  (for ([(k v) (in-hash meta-commands-with-keys)])
    (display (car v))
    (cond
      [(equal? k "0")
       (displayln "")
       (displayln "")]
      [else (display " ")]))
  (newline)
  (newline))

(define (print-choices-and-meta-commands-with-keys choices-with-keys
                                                   static-choices-with-keys
                                                   fragment-decisions-with-keys
                                                   meta-commands-with-keys)
  (print-decisions-with-keys fragment-decisions-with-keys)
  (print-choices-with-keys choices-with-keys)
  (print-static-choices-with-keys static-choices-with-keys)
  (print-meta-commands-with-keys meta-commands-with-keys))

(define (meta-command-valid? meta-commands-with-keys input)
  (set! input (string-upcase input))
  (define meta-command (hash-ref meta-commands-with-keys input '()))
  (if (not (null? meta-command)) meta-command #f))
