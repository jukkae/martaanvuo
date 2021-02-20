#lang racket

(require "actions.rkt")

; tables, items
(define *loot* '('amulet 'knife))

(define (get-inline-description item)
  (cond ((eq? item 'amulet) "a crow feather amulet")
        ((eq? item 'knife) "a blackstone knife")
        (else error "<unknown item!>")))

(define (get-list-inline-description list)
  (if (empty? list)
      "nothing"
      (string-append (get-inline-description (car list))
                     (cond ((= (length list) 1) "")
                           ((= (length list) 2) (string-append " and " (get-list-inline-description (cdr list))))
                           (else (string-append ", " (get-list-inline-description (cdr list))))))))

; dice shorthand
(define (d n sides)
  (for/sum ([i n])
    (add1 (random sides))))

; list utils
(define (take-n-random l n)
  (take (shuffle l) n))

(define (slice l offset n)
  (take (drop l offset) n))

(define (take-random l)
  (list-ref l (random (length l))))

; locations
(define location<%> (interface () get-description get-interactions get-visible-exits))
(define forest%
  (class* object% (location<%>)
    (define times-described 0)
    (define searched? #f)
    (super-new)

    (define/private (get-nth-description n)
      (cond ((= n 1) "You are walking through a dense coniferous forest. It is bitterly cold.")
            ((= n 2) "The Sun has come up a while ago. Her pale light scarcely filters through the branches of age-old pines.")
            ((= n 3) "You notice you are walking along a path, too narrow to be human, yet sharp, intelligent.")
            ((= n 4) "You are walking along a forest path.")
            ((= n 5) "You are walking along a forest path. You hear a RIVER to your right.")
            ((= n 6) "You are walking along a forest path. You hear a river to your right.")
            (else "You are utterly lost in the Dead Woods. Or was it the Woods of Dead? The woods seem very much alive to you.")))
      
    (define/public (get-description)
      (begin (set! times-described (add1 times-described))
             (get-nth-description *turn*)))

    (define/public (get-interactions) (if searched?
                                          null
                                          (list (make-action 'search "Search the surroundings." 3 null '(wilderness)))))

    (define/public (get-visible-exits)
      #(cond ((< times-described 5) (list (make-action 'go-on "Keep on walking." 1 null '(wilderness))))
             ((> times-described 4 (list (make-action "Keep on walking." 'go-on 1 null '(wilderness))
                                         (make-action "Go right." 'go-right 1 null '(wilderness)))))
             (else '()))
      (if (< times-described 5)
          (list (make-action 'go-on "Keep on walking." 1 null '(wilderness)))
          (list (make-action 'go-on "Keep on walking." 1 null '(wilderness)))))

    (define/public (search)
      (set! searched? #t)
      (define target-number 4)
      (define critical 10)
      (define roll (d 2 6))
      (define loot (cond ((> roll critical) 'amulet)
                         ((> roll target-number) 'knife)
                         (else 'nothing)))
      (newline)
      (displayln "The area looks promising, so you take a look around.")
      (displayln (string-append "-- Rolling 2d6, got " (number->string roll) "."))
      (cond ((eq? loot 'nothing) (displayln "You find nothing of interest."))
            (else
             (displayln (string-append "Ah ha! You find " (get-inline-description loot) " half buried under a spiritstone. A gift."))
             (displayln "You pick it up.")
             (set! *inventory* (cons loot *inventory*))
             )))
    (define/public (camp)
      (displayln "Gonna camp"))))

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
  (display "You have: ")
  (display (get-list-inline-description *inventory*)))

(define (describe-situation)
  (newline)
  (displayln (string-append "-- Paragraph " (number->string *turn*) ", elapsed time: " (number->string *time-elapsed*) " jiffies"))
  (newline)
  (when (not *in-combat*) (displayln (send *location* get-description)))
  (when *in-combat* (displayln (string-append "You are grappling with a Bloodleech. It has " (number->string *enemy-hp*) " HP."))))


(define (fight)
  (set! *in-combat* #t)
  (define to-hit (+ (d 2 6) *attack-skill*))
  (define target 8)
  (define damage (d 1 4))
  (newline)
  (displayln (take-random '("You go for a strike. Aim at the soft underbelly." "You lean in to strike. Put your weight behind it, pierce the scourge.")))
  (displayln (string-append "-- You rolled a " (number->string to-hit) "."))
  (cond ((>= to-hit target)
         (displayln (string-append "Your knife does " (number->string damage) " HP of damage to the Bloodleech."))
         (set! *enemy-hp* (- *enemy-hp* damage))
         (when (<= *enemy-hp* 0) (begin (set! *enemy-hp* 0) (set! *in-combat* #f)))
         (when (= *enemy-hp* 0) (displayln "The Bloodleech is dead.")))
        ((= to-hit target) (displayln "You barely miss."))
        (else (displayln "You miss."))))

(define (surrender)
  (set! *in-combat* #f)
  (displayln "You surrender!"))

(define (get-curse)
  (define first '("Rot" "Blight" "Pus"))
  (define second '("decay" "corrosion" "death" "degeneration"))
  (string-append (take-random first) " and " (take-random second) "!"))

(define *combat-options* (list (make-action 'strike "Strike. Hard." 1 *enemy-name* '(combat))
                               (make-action 'run "Run. Fast." 1 null '(combat))))

(define (update-state! action)
  (case (action-symbol action)
    ['search (begin (send *location* search)
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
    [(4) (set! *in-combat* true) (displayln (string-append (get-curse) " You get attacked by a " *enemy-name* "!"))]))

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
        (list (make-action 'inventory "Show inventory (free action)" 0 null '(always free)) ; tag - duration in jiffies - object - list of tags
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