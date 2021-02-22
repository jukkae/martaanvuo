#lang racket

(require "actions.rkt")
(require "creatures.rkt")
(require "items.rkt")
(require "locations.rkt")
(require "pc.rkt")
(require "utils.rkt")


(define *forest* (new forest%))

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
  (newline)
  (displayln (string-append "-- Paragraph " (number->string *turn*) ", elapsed time: " (number->string *time-elapsed*) " jiffies"))
  (newline)
  (when (not *in-combat*) (displayln (send *location* get-nth-description *turn*)))
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
                      ))
               (set! *time-elapsed* (add1 *time-elapsed*)))]
    ['inventory (print-inventory)]
    ['go-on (begin (newline)
                   (displayln (take-random '("Better get to it, then." "You keep on walking.")))
                   (set! *time-elapsed* (add1 *time-elapsed*)))]
    ['stab (begin (fight)
                  (set! *time-elapsed* (add1 *time-elapsed*)))]
    ['camp (displayln (take-random '("You are not tired." "You are barely getting started." "It is too early to camp.")))]
    ['run (newline) (displayln (take-random '("You try to run.")))]
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
           (displayln (string-append "A weapon would be nice.")))]))

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

(define (hang-until-valid-action actions)
  (newline)
  (display "Unknown command. Known commands: ")
  (for ([(k v) (in-hash actions)]) (display k))
  (display "Q") ; meta options
  (newline)
  (define input (read-line))
  ; meta-actions
  (handle-meta-actions input)
  ; otherwise
  (define command (hash-ref actions (string->number input) 'not-found))
  (if (equal? command 'not-found)
      (hang-until-valid-action actions)
      command))

(define (show-choices-and-get-action)
  (define options (build-keys-to-options-map))
  (newline)
  (for ([(k v) (in-hash options)])
    (displayln (string-append "[" (number->string k) "]: " (action-name v))))

  ; display meta actions
  (newline)
  (displayln "[Q]: Quit.")
  
  (define user-input (ask-input))

  ; meta-actions
  (handle-meta-actions user-input)
  ; otherwise
  (define command (hash-ref options (string->number user-input) 'not-found))
  (when (equal? command 'not-found)
    (set! command (hang-until-valid-action options)))
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

(define (handle-meta-actions input)
  (cond ((string-ci=? "Q" input) (quit))
        ((string-ci=? "R" input) (restart))
        (else 'continue)))

(define (end-game)
  (newline)
  (display "Do you want to try again? [Q] to quit, [R] to restart.")
  (define user-input (ask-input 'meta))

  ; meta-actions
  (when (equal? 'continue (handle-meta-actions user-input)) (error "UNHANDLED INPUT IN END-GAME")) ; beginnings of main menu, actually
  )

(define (restart) (meta-loop))

(define (meta-loop)
  ;begin new run
  (reset-meta)
  (newline)
  (displayln "A sense of self emerges from the Dark. You arise in M A R T A A N V U O.")
  (newline)
  (displayln (string-append "-- Run #" (number->string *metaloop*)))

  (resolve-turn)
  (displayln "UNHANDLED"))

(define (startup)
  (title)
  (newline)
  (displayln "You should be able to select a previous save. You can not.")
  (newline)
  (displayln "Let's begin.")
  (call/cc (end-game (meta-loop))))

(startup)