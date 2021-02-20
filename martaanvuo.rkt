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
(define *metaloop* 1)
(define *turn* 1)
(define *time-elapsed* 0)
(define *hp* 4)
(define *attack-skill* 1)
(define *creatures* '())

(define (print-inventory)
  (newline)
  (displayln "You ponder your earthly possessions.")
  (newline)
  (displayln (string-append "In addition to clothes, you have " (get-list-inline-description (get-field inventory *pc*)) ".")))

(define (describe-situation)
  (newline)
  (displayln (string-append "-- Paragraph " (number->string *turn*) ", elapsed time: " (number->string *time-elapsed*) " jiffies"))
  (newline)
  (when (not *in-combat*) (displayln (send *location* get-nth-description *turn*)))
  (when *in-combat* (displayln (string-append "You are grappling with a Bloodleech. It has " (number->string (get-field hp *creatures*)) " HP."))))


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
              (take-random '("You go for a strike. Aim at the soft underbelly."
                             "You lean in to strike. Put your weight behind it, pierce the scourge."
                             "You slash."
                             "You strike."
                             "You go for a strike."
                             "You lean in to strike."))
              " [2d6+1]: "
              (number->string to-hit)))
  (cond ((>= to-hit target)
         (displayln (take-random '("Your strike connects."
                                   "Your strike lands with a satisfying thud."
                                   "Your blade pierces the skin of the enemy.")))
         (displayln (string-append "[damage: " (number->string damage) " HP]"))
         (define result (send *creatures* hit damage))
         (when (equal? result 'dead) (begin (displayln "The enemy is dead.")
                                             (set! *in-combat* #f))))
        (else (displayln (string-append (get-curse) " You miss.")))))

(define (get-curse)
  (define index (random 2))
  (cond ((= index 0) (define first '("Rot" "Blight" "Pus"))
                     (define second '("decay" "corrosion" "death" "destruction"))
                     (string-append (take-random first) " and " (take-random second) "!"))
        (else (take-random '("Let everything wither!"
                             "Blight!"
                             "Scales of a snake!")))))

(define (update-state! action)
  (case (action-symbol action)
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
                   (displayln (take-random '("Better get to it, then." "You go on." "Your boots tread the ground." "You keep on walking.")))
                   (set! *time-elapsed* (add1 *time-elapsed*)))]
    ['strike (begin (fight)
                    (set! *time-elapsed* (add1 *time-elapsed*)))]
    ['camp (displayln (take-random '("You are not tired." "You are barely getting started." "It is too early to camp.")))]
    ['run (newline) (displayln (take-random '("You try to run.")))]
    [else (error "Unknown action!")]))

(define (run-on-turn-actions . turn)
  (case *turn*
    [(3) (spawn-enemy)
         (set! *in-combat* true)
         (newline)
         (displayln (string-append (get-curse) " You get attacked by a " (send *creatures* get-name) "!"))]))

(define (key-from-index i)
  (cond ((< i 0) (error "negative index!"))
        ((<= i 8) (add1 i))
        ((= i 9) 0)
        ((> i 9) (error "too many things to do!"))))

(define (build-keys-to-options-map)
  (define (player-has-weapons?) (filter
                               (lambda (item) (member 'strike (send item get-uses)))
                               (get-field inventory *pc*)))
  (define location-options (send *location* get-interactions))
  (define next-location-choices (send *location* get-visible-exits))
  (define combat-options
    (if *in-combat*
        (if (player-has-weapons?)
            (list (make-action 'strike "Strike." 1 *creatures* '(combat))
                  (make-action 'run "Run." 1 null '(combat)))
            (list (make-action 'run "Run." 1 null '(combat))))
        null))
  (define generic-options
    (if (not (empty? (get-field inventory *pc*)))
        (list (make-action 'inventory "Show inventory [free action]" 0 null '(always free)) ; tag - duration in jiffies - object - list of tags
              (make-action 'break-from-game-loop "Quit." 0 null '(always)))
        (list (make-action 'break-from-game-loop "Quit." 0 null '(always)))))

  (define options (append location-options next-location-choices combat-options generic-options))
  (when *in-combat* (set! options (filter is-combat? options)))
  (define options-hash (make-hash))

  (for ([i (in-range (length options))])
    (define key (key-from-index i))
    (hash-set! options-hash key (list-ref options i)))
  options-hash)

(define (ask-input)
  (newline)
  (displayln "What do you do?")
  (newline)
  (read-line))

(define (show-choices-and-get-action)
  (define options (build-keys-to-options-map))  
  (newline)
  (for ([(k v) (in-hash options)])
    (displayln (string-append "[" (number->string k) "]: " (action-name v))))
  (define user-input (ask-input))
  (define command (hash-ref options (string->number user-input)))
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
  (displayln "M A R T A A N V U O"))

(title)
(resolve-turn)