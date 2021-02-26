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
(define *world* (new world%))
(define *metaloop* 0)
(define *turns-total* 1)

(define *show-meta-commands* #t)

(define (reset-meta)
  (reset-state)
  (set! *world* (new world%))
  (set! *metaloop* (add1 *metaloop*)))

(define (describe-situation)
  (newline)
  (displayln (string-append "-- Turn " (number->string *turn*) ", elapsed time: " (number->string *time-elapsed*) " jiffies"))
  (newline)
  (when (not *in-combat*) (displayln (send *location* get-description)))
  (when (and (not *in-combat*) (= *turn* 2))
    (begin
      (newline)
      (displayln (send *pc* get-a-hunch))))
  (when *in-combat* (displayln (string-append "You are grappling with a " (send *creatures* get-name) ". [" (number->string (get-field hp *creatures*)) " HP]"))))

(define (fight)
  (send *world* set-combat #t)
  (define to-hit (+ (d 2 6) (get-field attack-skill *pc*)))
  (define target (get-field defense *creatures*))
  (define damage (d 1 4))
  (newline)
  (displayln (string-append
              (take-random '("You go for a stab. Aim at the soft underbelly."
                             "You lean in to stab. Put your weight behind it, pierce the scourge."
                             "You slash."
                             "You stab."
                             "You go for a stab."
                             "You lean in to stab."))
              " [2d6+1: "
              (number->string to-hit)
              "]"))
  (cond ((>= to-hit target)
         (displayln (take-random '("Your stab connects."
                                   "Your stab lands with a satisfying thud."
                                   "Your blade pierces the skin of the enemy.")))
         (displayln (string-append "[damage: " (number->string damage) " HP]"))
         (define result (send *creatures* hit damage))
         (when (equal? result 'dead) (begin (displayln (string-append "The " (send *creatures* get-name) " is dead."))
                                            (send *world* set-combat #f))))
        (else (displayln (string-append (get-curse) " You miss.")))))

(define (brawl)
  (send *world* set-combat #t)
  (define to-hit (+ (d 2 6) (get-field attack-skill *pc*) 2)) ; +2 to hit bonus; having +defense against this opponent would be great
  (define target (get-field defense *creatures*))
  (define damage (d 1 2))
  (newline)
  (displayln (string-append
              (take-random '("You grapple with the enemy. Try to get it pinned."
                             "You wrestle."
                             "You try to get it pinned. That is the Way of the Anthead of Riverfrost."
                             #;"You try to strangle."
                             #;"Get it pinned, then skinned."
                             #;"Get it pinned, then skinned. Came up with that myself."
                             #;"Not the first neck I've broken."))
              " [2d6+1: "
              (number->string to-hit)
              "]"))
  (cond ((>= to-hit target)
         (displayln (take-random '("Snap. You feel a crack under your fingers."
                                   "Crunch."
                                   "Crack."
                                   "Cronk. You feel something thick break under your hands."
                                   "Puff. The snow billows up as you throw down your enemy under you.")))
         (displayln (string-append "[damage: " (number->string damage) " HP]"))
         (define result (send *creatures* hit damage))
         (when (equal? result 'dead) (begin (displayln (string-append "The " (send *creatures* get-name) " is dead."))
                                            (send *world* set-combat #f))))
        (else (displayln (string-append (get-curse) " You can't get a good hold of the enemy.")))))

(define (quit)
  (newline)
  (displayln "You quit. For now.")
  (newline)
  (displayln "Your progress should be saved. It is not.")
  (exit))

(define (update-state! action)
  (case (action-symbol action)
    ['quit (quit)]
    ['search (begin
               (define loot (send *location* search))
               (cond ((eq? loot 'nothing) (displayln "You find nothing of interest."))
                     (else
                      (newline)
                      (displayln (string-append "Ah ha! You find " (send loot get-inline-description) " half buried under a rock. A gift."))
                      (newline)
                      (displayln (string-append "You pick up the " (send loot get-short-description) "."))
                      (set-field! inventory *pc* (cons loot (get-field inventory *pc*)))
                      (when (is-a? loot figurine%) (win))
                      ))
               (send *world* advance-time))]
    ['inventory (print-inventory (get-list-inline-description (get-field inventory *pc*)))]
    ['go-on (begin (newline)
                   (displayln (take-random '("Better get to it, then." "You keep on walking.")))
                   (send *world* advance-time))]
    ['stab (begin (fight)
                  (send *world* advance-time))]
    ['brawl (begin (brawl)
                   (send *world* advance-time))]
    ['camp (displayln (take-random '("You are not tired." "You are barely getting started." "It is too early to camp.")))]
    ['run (newline) (displayln (take-random '("You try to run.")))]
    ['go-to-mountains (error "implement go to-action")]
    ['go-to-river (error "implement go to-action")]
    ['go-downriver (error "implement go to-action")]
    [else (error (string-append "Unknown action: " (symbol->string (action-symbol action))))]))

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
  (when (equal? 'continue (handle-meta-actions user-input meta-options #t)) (hang-until-valid-action (make-hash) meta-options))
  )

(define (win)
  (newline)
  (display "You found what you sought. You win the game and die of old age. [Q] to quit, [R] to restart.")
  (define user-input (ask-input 'meta))

  ; meta-actions
  (define meta-options (make-hash))
  (hash-set! meta-options "Q" (cons "[Q]: Quit." quit))
  (hash-set! meta-options "R" (cons "[R]: Restart." restart))
  (when (equal? 'continue (handle-meta-actions user-input meta-options #t)) (hang-until-valid-action (make-hash) meta-options))
  )

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