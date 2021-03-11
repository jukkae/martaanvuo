#lang racket

(require dyoo-while-loop)

(require "actions.rkt")
(require "actors.rkt")
(require "creatures.rkt")
(require "items.rkt")
(require "locations.rkt")
(require "narration.rkt")
(require "pc.rkt")
(require "utils.rkt")
(require "world.rkt")

; globals and state
(define *world* '())
(define *metaloop* 0)

(define (reset-meta)
  (set! *world* (make-new-world))
  (set! *metaloop* (add1 *metaloop*)))

(define (quit)
  (narrate-quit)
  (exit))

(define (key-from-index i)
  (cond ((< i 0) (error "negative index!"))
        ((<= i 8) (add1 i))
        ((= i 9) 0)
        ((> i 9) (error "too many things to do!"))))

(define (build-keys-to-choices-map choices)
  (define choices-with-keys (make-hash))
  (for ([i (in-range (length choices))])
    (define key (key-from-index i))
    (hash-set! choices-with-keys key (list-ref choices i)))
  choices-with-keys)

(define (get-meta-commands-with-keys)
  (define meta-commands (make-hash))
  (hash-set! meta-commands "Q" (cons "[Q]: Quit." quit))
  meta-commands)

(define (wait-for-input)
  (define input (read-line))
  input)

(define (print-choices-with-keys choices-with-keys)
  (for ([(k v) (in-hash choices-with-keys)])
    (displayln (string-append "[" (number->string k) "]: " (choice-name v))))
  (newline))

(define (print-meta-commands-with-keys meta-commands-with-keys)
  (for ([(k v) (in-hash meta-commands-with-keys)])
    (display (car v)))
  (newline)
  (newline))

(define (run-meta-command meta-command-with-key)
  (define meta-command (cdr meta-command-with-key))
  (meta-command))

(define (try-to-handle-as-meta-command valid-meta-commands-with-keys input)
  (set! input (string-upcase input))
  (define meta-command (hash-ref valid-meta-commands-with-keys input '()))
  (if (not (null? meta-command))
      (begin (run-meta-command meta-command)
             #t)
      #f))

(define (try-to-handle-as-choice valid-choices-with-keys input)
  (define choice (hash-ref valid-choices-with-keys (string->number input) '()))
  (if (not (null? choice))
      choice
      #f))

(define (pebkac-loop choices-with-keys meta-commands-with-keys)
  (display "Unknown command. Known commands: ")
  (for ([(k v) (in-hash choices-with-keys)]) (display k))
  (for ([(k v) (in-hash meta-commands-with-keys)]) (display k))
  (newline)
  
  (define input (wait-for-input))
  (define handled? '())
  (while (null? handled?)
         (set! handled? (try-to-handle-as-meta-command meta-commands-with-keys input))
         (when (not handled?)
           (set! handled? (try-to-handle-as-choice choices-with-keys input)))
         (when (not handled?)
           (set! handled? (pebkac-loop choices-with-keys meta-commands-with-keys))))
  handled?)

(define (get-next-action actor)
  (cond ((is-a? actor pc%)
         ;(displayln (syntax-source #'here))
         ;(displayln (syntax-line #'here))
         (define choices (get-world-choices *world* actor))
         (define choices-with-keys (build-keys-to-choices-map choices))
         (print-choices-with-keys choices-with-keys)

         (define meta-commands-with-keys (get-meta-commands-with-keys))
         (print-meta-commands-with-keys meta-commands-with-keys)

         (displayln "What do you do?")
         (newline)

         (define input (wait-for-input))

         (define handled? (try-to-handle-as-meta-command meta-commands-with-keys input))
         (when (not handled?)
           (set! handled? (try-to-handle-as-choice choices-with-keys input)))
         (when (not handled?)
           (set! handled? (pebkac-loop choices-with-keys meta-commands-with-keys)))

         ; handled? should now contain a valid action
         (unless handled? (error "Input not handled even in PEBKAC loop!")) ; assert that handled? is truthy - TODO implement assert!
    
         (define choice handled?) ; ta-dah
         choice ; choice is a PC-only concept, and acts as a template for an action
         (define action (make-action-from-choice *world* choice))
         action
         )
        (else
         (define action (send actor get-next-action))
         action)))


(define (resolve-turn)
  (begin-turn! *world*)
  (describe-situation *world*)
  (on-turn! *world*)
  
  (define actions '())
  (define current-location (get-field current-location *world*))
  (define actors (get-field actors current-location))
  
  (for ([i (in-range (length actors))])
    (define actor (list-ref actors i))
    (define action (get-next-action actor))

    (if (resolve-instantly? action)
        (resolve-action! *world* action)
        (add-action-to-queue *world* action)))

  (resolve-actions! *world*)

  (end-turn! *world*)
  (resolve-turn))




(define (end-game)
  (newline)
  (display "Do you want to try again? [Q] to quit, [R] to restart."))


(define (restart) (meta-loop))

(define (meta-loop)
  ;begin new run
  (reset-meta)
  (narrate-run-number *metaloop*)

  (resolve-turn)
  (error "meta-loop: resolve-turn should not exit recursion"))

(define (startup)
  (title)
  (narrate-startup)
  (call/cc (end-game (meta-loop))))

(startup)