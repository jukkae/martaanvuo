#lang racket

(require "actions.rkt")
(require "creatures.rkt")
(require "items.rkt")
(require "locations.rkt")
(require "narration.rkt")
(require "pc.rkt")
(require "utils.rkt")
(require "world.rkt")

; globals and state
(define *metaloop* 0)
(define *turns-total* 1)

(define *show-meta-commands* #t)

(define (reset-meta)
  (reset-state)
  (set! *metaloop* (add1 *metaloop*)))

(define (quit)
  (narrate-quit)
  (exit))

(define (key-from-index i)
  (cond ((< i 0) (error "negative index!"))
        ((<= i 8) (add1 i))
        ((= i 9) 0)
        ((> i 9) (error "too many things to do!"))))

(define (build-keys-to-options-map)
  (define location-options (send *location* get-interactions))
  (define next-location-choices (send *location* get-visible-exits))
  (define combat-options
    (if *in-combat*
        (if (player-has-weapons?)
            (list (make-action 'stab "Stab." 1 *creatures* '(combat))
                  (make-action 'brawl "Brawl." 1 *creatures* '(combat))
                  (make-action 'run "Run." 1 null '(combat)))
            (list (make-action 'brawl "Brawl." 1 *creatures* '(combat))
                  (make-action 'run "Run." 1 null '(combat))))
        null))
  (define generic-options
    (if (not (empty? (get-field inventory *pc*)))
        (list (make-action 'inventory "Show inventory. [free action]" 0 null '(always free))) ; tag - duration in jiffies - object - list of tags
        '()))

  (define options (append location-options next-location-choices combat-options generic-options))
  (when *in-combat* (set! options (filter is-combat? options)))
  (define options-hash (make-hash))

  (for ([i (in-range (length options))])
    (define key (key-from-index i))
    (hash-set! options-hash key (list-ref options i)))
  options-hash)

(define (ask-input . context?)
  (newline)
  (cond [(equal? context? 'meta) (display "Try again? [Q] to quit, [R] to restart.")]
        [(null? context?) (displayln "What do you do?")])
  (newline)
  (read-line))

(define (toggle-meta)
  (set! *show-meta-commands* (not *show-meta-commands*))
  (newline)
  (displayln "Hid meta commands, [S] to show again."))

(define (hang-until-valid-action actions meta-actions)
  (newline)
  (display "Unknown command. Known commands: ")
  (for ([(k v) (in-hash actions)]) (display k))
  (for ([(k v) (in-hash meta-actions)]) (display k))
  (newline)
  (define input (read-line))
  ; meta-actions
  (handle-meta-actions input meta-actions)
  ; otherwise
  (define command (hash-ref actions (string->number input) 'not-found))
  (if (equal? command 'not-found)
      (hang-until-valid-action actions meta-actions)
      command))

; TODO this needs a rewrite in terms of commands.
; Commands can either be meta commands, in which case they should get letters,
; or regular commands, in which case they should get numbers.
; If it's neither, then it's a special value that throws the input handler
; into the PEBKAC loop.
; Meta commands should be handled at this level, regular commands should be
; passed on to "Game Manager".
;
; Are Commands and Actions the same thing? Likely not. Showing inventory
; feels like a command that happens immediately and doesn't affect the world,
; whereas actions are something that can be queued and that take time to resolve.
(displayln "TODO: find me and fix me")
(define (show-choices-and-get-action)
  (define options (build-keys-to-options-map))
  (newline)
  (for ([(k v) (in-hash options)])
    (displayln (string-append "[" (number->string k) "]: " (action-name v))))

  ; display meta actions
  (define meta-options (make-hash))
  (hash-set! meta-options "Q" (cons "[Q]: Quit." quit))
  #;(hash-set! meta-options "S" (cons "[S]: Show/hide meta commands." toggle-meta))
  (newline)
  (for ([(k v) (in-hash meta-options)])
    (display (car v)))
  (newline)

  (define user-input (ask-input))

  (handle-meta-actions user-input meta-options)

  ; not handled yet
  (define command (hash-ref options (string->number user-input) 'not-found))
  (when (equal? command 'not-found)
    (set! command (hang-until-valid-action options meta-options)))
  command)

(define (resolve-turn)
  (describe-situation)
  (run-on-turn-actions)
  
  (define action (show-choices-and-get-action))
  (define result (update-state! action))
  (when (not (is-free? action))
    (send *world* advance-turn)
    (send *location* advance-to-next-description!))
  (cond ((equal? result 'u-ded) (newline) 'end-game)
        (else (resolve-turn))))

(define (handle-meta-actions input meta-actions . hang-on-not-found?)
  (set! input (string-upcase input))
  (define meta-action (hash-ref meta-actions input 'not-found))
  (if (equal? meta-action 'not-found)
      (if (not (null? hang-on-not-found?))
          (hang-until-valid-action (make-hash) meta-actions)
          'continue)
      ((cdr meta-action))))

(define (end-game)
  (newline)
  (display "Do you want to try again? [Q] to quit, [R] to restart.")
  (define user-input (ask-input 'meta))

  ; meta-actions
  (define meta-options (make-hash))
  (hash-set! meta-options "Q" (cons "[Q]: Quit." quit))
  (hash-set! meta-options "R" (cons "[R]: Restart." restart))
  (when (equal? 'continue (handle-meta-actions user-input meta-options #t)) (hang-until-valid-action (make-hash) meta-options)))

(define (win)
  (newline)
  (display "You found what you sought. You win the game and die of old age. [Q] to quit, [R] to restart.")
  (define user-input (ask-input 'meta))

  ; meta-actions
  (define meta-options (make-hash))
  (hash-set! meta-options "Q" (cons "[Q]: Quit." quit))
  (hash-set! meta-options "R" (cons "[R]: Restart." restart))
  (when (equal? 'continue (handle-meta-actions user-input meta-options #t)) (hang-until-valid-action (make-hash) meta-options)))

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