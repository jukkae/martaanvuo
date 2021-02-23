#lang racket

(require roman-numeral)

(require "actions.rkt")
(require "creatures.rkt")
(require "items.rkt")
(require "locations.rkt")
(require "pc.rkt")
(require "utils.rkt")


(define *forest* (new forest%))
(define *mountains* (new mountains%))

; globals and state
(define *pc* (new pc%))
(define *location* *forest*)
(define *in-combat* #f)
(define *metaloop* 0)
(define *turn* 1)
(define *turns-total* 1)
(define *time-elapsed* 0)
(define *hp* 4)
(define *attack-skill* 1)
(define *creatures* '())

(define *show-meta-commands* #t)

(define (reset-meta)
  (set! *pc* (new pc%))
  (set! *location* *forest*)
  (set! *in-combat* #f)
  (set! *metaloop* (add1 *metaloop*))
  (set! *turn* 1)
  (set! *time-elapsed* 0)
  (set! *hp* 4)
  (set! *attack-skill* 1)
  (set! *creatures* '()))

(define (print-inventory)
  (newline)
  (displayln "You ponder your earthly possessions.")
  (newline)
  (displayln (string-append "In addition to clothes, you have " (get-list-inline-description (get-field inventory *pc*)) ".")))

(define (describe-situation)
  (newline)
  (displayln (string-append "-- Turn " (number->string *turn*) ", elapsed time: " (number->string *time-elapsed*) " jiffies"))
  (newline)
  (when (not *in-combat*) (displayln (send *location* get-nth-description *turn*)))
  (when (and (not *in-combat*) (= *turn* 2))
    (begin
      (newline)
      (displayln (send *pc* get-a-hunch))))
  (when *in-combat* (displayln (string-append "You are grappling with a " (send *creatures* get-name) ". [" (number->string (get-field hp *creatures*)) " HP]"))))


(define (spawn-enemy)
  (define r (random 2))
  (define enemy (cond ((= r 0) (new bloodleech%))
                      (else (new blindscraper%))))
  (set! *creatures* enemy))

(define (fight)
  (set! *in-combat* #t)
  (define to-hit (+ (d 2 6) *attack-skill*))
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
                                            (set! *in-combat* #f))))
        (else (displayln (string-append (get-curse) " You miss.")))))

(define (brawl)
  (set! *in-combat* #t)
  (define to-hit (+ (d 2 6) *attack-skill* 2)) ; +2 to hit bonus; having +defense against this opponent would be great
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
                                            (set! *in-combat* #f))))
        (else (displayln (string-append (get-curse) " You can't get a good hold of the enemy.")))))

(define (get-curse)
  (define index (random 2))
  (cond ((= index 0) (define first '("Rot" "Blight" "Pus" "Pain"))
                     (define second '("decay" "corrosion" "death" "destruction" "sorrow"))
                     (string-append (take-random first) " and " (take-random second) "!"))
        (else (take-random '("Let it all wither!"
                             "May it all languish!"
                             "Blight!"
                             "Scales of a snake!")))))

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
               (set! *time-elapsed* (add1 *time-elapsed*)))]
    ['inventory (print-inventory)]
    ['go-on (begin (newline)
                   (displayln (take-random '("Better get to it, then." "You keep on walking.")))
                   (set! *time-elapsed* (add1 *time-elapsed*)))]
    ['stab (begin (fight)
                  (set! *time-elapsed* (add1 *time-elapsed*)))]
    ['brawl (begin (brawl)
                   (set! *time-elapsed* (add1 *time-elapsed*)))]
    ['camp (displayln (take-random '("You are not tired." "You are barely getting started." "It is too early to camp.")))]
    ['run (newline) (displayln (take-random '("You try to run.")))]
    ['go-to-mountains (begin (newline)
                             (set! *location* *mountains*)
                             )]
    [else (error "Unknown action!")]))

(define (run-on-turn-actions . turn)
  (when *in-combat*
    (newline)
    (displayln (string-append "The " (send *creatures* get-name) " attacks you."))
    (define to-hit (+ (d 2 6) 1))
    (define target 6)
    (define damage (d 1 2))
    (displayln (string-append "[to hit: 2d6+1: " (number->string to-hit) "]"))
    (if (> to-hit target)
        (begin (displayln (string-append "[dmg: 1d2: " (number->string damage) "]"))
               (displayln "Oof. That hurt.")
               (send *pc* hit damage)
               (if (<= (get-field hp *pc*) 0)
                   (begin (displayln "You are dead.")
                          (end-game))
                   (displayln (string-append "You have " (number->string (get-field hp *pc*)) "HP."))))
        (begin (displayln "You dodge."))))

  (case *turn*
    [(3) (spawn-enemy)
         (set! *in-combat* true)
         (newline)
         (displayln (string-append (get-curse) " A " (send *creatures* get-name) " crawls forth. It looks at you like you would make a tasty meal for it."))
         (when (not (player-has-weapons?))
           (newline)
           (displayln (string-append "A weapon would be nice. But your hands are strong, and every living thing lives the same.")))]))

(define (key-from-index i)
  (cond ((< i 0) (error "negative index!"))
        ((<= i 8) (add1 i))
        ((= i 9) 0)
        ((> i 9) (error "too many things to do!"))))

(define (player-has-weapons?) (not (empty? (filter
                                            (lambda (item) (member 'stab (send item get-uses)))
                                            (get-field inventory *pc*)))))

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
  (when (not (is-free? action)) (set! *turn* (add1 *turn*)))
  (cond ((equal? result 'u-ded) (newline) 'end-game)
        (else (resolve-turn))))

(define (title)
  (newline)
  (displayln "M A R T A A N V U O")
  (displayln "==================="))

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
  (newline)
  (newline)
  (displayln (string-append "BOOK " (string-upcase (number->roman *metaloop*))))
  (newline)
  (displayln "A sense of self emerges from the Dark. You arise in")
  (displayln "M A R T A A N V U O.")
  (newline)
  

  (resolve-turn)
  (displayln "UNHANDLED"))

(define (startup)
  (title)
  (newline)
  (displayln "You should be able to select a previous save. You can not.")
  (newline)
  (displayln "A new game begins.")
  (call/cc (end-game (meta-loop))))

(startup)