#lang racket

(require "actions.rkt")
(require "creatures.rkt")
(require "items.rkt")
(require "locations.rkt")
(require "utils.rkt")


(define *forest* (new forest%))

; globals and state
(define *location* *forest*)
(define *in-combat* #f)
(define *enemy-name* "Bloodleech")
(define *enemy-hp* 2)
(define *metaloop* 1)
(define *turn* 1)
(define *time-elapsed* 0)
(define *hp* 4)
(define *attack-skill* 1)
(define *inventory* '())

(define (print-inventory)
  (newline)
  (displayln "You ponder your earthly possessions.")
  (newline)
  (displayln (string-append "In addition to clothes, you have " (get-list-inline-description *inventory*) ".")))

(define (describe-situation)
  (newline)
  (displayln (string-append "-- Paragraph " (number->string *turn*) ", elapsed time: " (number->string *time-elapsed*) " jiffies"))
  (newline)
  (when (not *in-combat*) (displayln (send *location* get-nth-description *turn*)))
  (when *in-combat* (displayln (string-append "You are grappling with a Bloodleech. It has " (number->string *enemy-hp*) " HP."))))


(define (fight)
  (set! *in-combat* #t)
  (define to-hit (+ (d 2 6) *attack-skill*))
  (define target 5)
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
         (displayln (string-append "Your knife does " (number->string damage) " HP of damage to the Bloodleech."))
         (set! *enemy-hp* (- *enemy-hp* damage))
         (when (<= *enemy-hp* 0) (begin (set! *enemy-hp* 0) (set! *in-combat* #f)))
         (when (= *enemy-hp* 0) (displayln "The Bloodleech is dead.")))
        ((= to-hit target) (displayln "You barely miss."))
        (else (displayln "You miss."))))

(define (get-curse)
  (define first '("Rot" "Blight" "Pus"))
  (define second '("decay" "corrosion" "death" "degeneration"))
  (string-append (take-random first) " and " (take-random second) "!"))

(define *combat-options* (list (make-action 'strike "Strike. Hard." 1 *enemy-name* '(combat))
                               (make-action 'run "Run. Fast." 1 null '(combat))))

(define (update-state! action)
  (case (action-symbol action)
    ['search (begin
               (define loot (send *location* search))
               (cond ((eq? loot 'nothing) (displayln "You find nothing of interest."))
                     (else
                      (newline)
                      (displayln (string-append "Ah ha! You find " (send loot get-inline-description) " half buried under a spiritstone. A gift."))
                      (newline)
                      (displayln "You pick it up.")
                      (set! *inventory* (cons loot *inventory*))
                      ))
               (set! *time-elapsed* (add1 *time-elapsed*)))]
    ['inventory (print-inventory)]
    ['go-on (begin (newline)
                   (displayln (take-random '("Better get to it, then." "You go on." "Your boots tread the ground." "You keep on walking.")))
                   (set! *time-elapsed* (add1 *time-elapsed*)))]
    ['strike (begin (fight)
                    (set! *time-elapsed* (add1 *time-elapsed*)))]
    ['camp (displayln (take-random '("You are not tired." "You are barely getting started." "It is too early to camp.")))]
    [else (error "Unknown action!")]))

(define (run-on-turn-actions . turn)
  (case *turn*
    [(3) (set! *in-combat* true) (newline) (displayln (string-append (get-curse) " You get attacked by a " *enemy-name* "!"))]))

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
        *combat-options*
        null))
  (define generic-options
    (if (not (empty? *inventory*))
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