#lang racket

(require dyoo-while-loop)

(require "actions.rkt")
(require "actors.rkt")
(require "creatures.rkt")
(require "items.rkt")
(require "locations.rkt")
(require "narration.rkt")
(require "utils.rkt")
(require "pc.rkt")


(define times-of-day '(morning afternoon evening night))
(define (get-next-time-of-day time-of-day)
  (cond ((eq? time-of-day 'morning) 'afternoon)
        ((eq? time-of-day 'afternoon) 'evening)
        ((eq? time-of-day 'evening) 'night)
        ((eq? time-of-day 'night) 'morning)
        (else error "get-next-time-of-day: not time of day")))

(define world%
  (class* object% ()
    (field [turn 0]) ; meta
    (field [status 'active]) ; meta - possible values currently 'active and 'ended
    (field [in-combat #f]) ; meta - situational
    (field [elapsed-time 0]) ; jiffies - jiffy is a relative unit that progresses during and between scenes. For downtime, 100 jiffies = 1 TOD

    ; underground: let jiffies drift off from physical needs!

    

    (field [time-of-day 'morning])
    (field [locations (make-hash)])
    (field [current-location '()]) ; meta

    (field [pc (new pc%)]) ; meta

    (field [action-queue '()])

    (super-new)

    (define/public (sort-actions!)
      (set! action-queue (sort
                          action-queue
                          action-faster-than)))
    (define/public (clear-action-queue!)
      (set! action-queue '()))

    (define/public (add-location index location)
      (hash-set! locations index location))


    (define/public (get-current-enemies)
      (define all-actors (location-actors current-location))
      (set! all-actors (remove pc all-actors))
      all-actors)

    ; return true if first is less, ie., sorted earlier, than second
    ; ie., #t = action1 is faster than action2
    (define (action-faster-than action1 action2)
      (cond ((has-tag? action1 'slow) #f)
            ((has-tag? action1 'slow) #t)
            ((eq? (action-actor action1) 'pc) #t)
            ((eq? (action-actor action2) 'pc) #f)))))



(define (make-new-world)
  (define world (new world%))
  (define pc (get-field pc world))
  (define location
    (make-location
     #:actors '()
     #:features '()
     #:items '()
     #:neighbors '()
     #:tags '()
     #:type 'swamp))
  (send world add-location 0 location)

  (set-pc-location! world pc location)
  world)

(define (make-neighbors location)
  (define number (d 1 3))
  (for/list ([i number])
    (define new-location
      (make-location
       #:actors '()
       #:features '()
       #:items '()
       #:neighbors '()
       #:tags '()
       #:type 'land))
    (set-location-neighbors! location (cons new-location (location-neighbors location)))
    ; routes are bidirectional
    (set-location-neighbors! new-location (cons location (location-neighbors new-location))))
  )

(define (set-pc-location! world pc location)
  (when (not (location-visited location))
    (make-neighbors location)
    )
  (set-field! current-location world location)
  (add-actor-to-location! location pc)
  (set-location-visited! location #t))

(define (advance-time-by-a-jiffy! world)
  (define events '())
  (define new-elapsed-time (add1 (get-field elapsed-time world)))
  (set-field! elapsed-time
              world
              new-elapsed-time)
    
  (when (= (modulo (get-field elapsed-time world) 100) 0)
    (set-field! time-of-day
                world
                (get-next-time-of-day
                 (get-field time-of-day world)))
    (displayln (string-append "It is now " (symbol->string (get-field time-of-day world)) ".")))

  (when (= (modulo (get-field elapsed-time world) 150) 130)
    (define roll (d 1 4))
    (cond ((= roll 1)
           (spawn-enemies world 2)
           (set! events (cons 'enemies-spawned events)))
          (else
           (displayln "It's eerily quiet."))))
  events
  )

(define (advance-time-until-next-interesting-event! world jiffies)
  (let/ec return
    (for ([t jiffies])
      (define event (advance-time-by-a-jiffy! world))
      (when (not (eq? event '()))
        (return event)))
    '()))

(define (begin-turn! world)
  (set-field! turn
              world
              (add1 (get-field turn world)))

  (newline)
  (displayln
   (string-append
    "-- Turn " (number->string (get-field turn world))
    ", "
    "elapsed time: " (number->string (get-field elapsed-time world)) " jiffies"
    ", "
    "location: " (number->string (location-id (get-field current-location world)))
    ", "
    "time of day: " (symbol->string (get-field time-of-day world))
    ))
  (newline))

(define (on-turn! world)
  '()
  #;(cond ((and
            (= (modulo (get-field turn world) 3) 0)
            (not (get-field in-combat world)))
           (begin
             (define challenge-rating (d 1 2))
             (spawn-enemies world challenge-rating)))))

(define (end-turn! world)
  (define pc (get-field pc world))
  (send pc clear-statuses!)

  (define enemies (send world get-current-enemies))
  (when (= (length enemies) 0)
    (set-field! in-combat world #f))
  '())



; This should be reimplemented in terms of describable<%> or something
(define (get-enemies-description enemies)
  (define enemies-in-bins (make-hash))
  (for ([i (in-range 0 (length enemies))])
    (define enemy (list-ref enemies i))
    (define key (send enemy get-name))

    (define similar-enemies (hash-ref enemies-in-bins key '()))
    
    (set! similar-enemies (cons enemy similar-enemies))
    (hash-set! enemies-in-bins key similar-enemies))
  
  (define number-of-keys (length (hash-keys enemies-in-bins)))

  (define (describe-group number-of-enemies name)
    (define name-length (length (string->list name)))
    (define first-character (substring name 0 1))
    (define last-character (substring name (- name-length 2) (- name-length 1)))

    (define (get-indefinite-article name)
      (match first-character
        [(regexp #rx"[aeiouyAEIOUY]") "an"]
        [_ "a"]))
    (define numeral
      (cond ((= number-of-enemies 1) (get-indefinite-article name))
            ((= number-of-enemies 2) "Two")
            ((= number-of-enemies 3) "Three")
            (else "Some")))
    (define inflected-name
      (cond ((= number-of-enemies 1) name)
            (else (match last-character
                    [(regexp #rx"[aeiouyAEIOUY]") (string-append name "s")]
                    [_ (string-append name "es")]))))
    (string-append numeral " " inflected-name))

  (define group-descriptions '())
  (for ([i (in-range 0 number-of-keys)])
    (define key (list-ref (hash-keys enemies-in-bins) i))
    (define enemies-in-bin (hash-ref enemies-in-bins key))
    
    (define number-of-enemies-in-bin (length enemies-in-bin))
    (define group-description (describe-group number-of-enemies-in-bin key))
    (set! group-descriptions (cons group-description group-descriptions)))
  (define enemies-description (get-string-list-inline-description group-descriptions))
  
  enemies-description)

(define (get-enemies-on-spawn-message enemies)
  (define enemies-list-description (get-enemies-description enemies))
  (define on-spawn-message
    (string-append (get-curse) "! " enemies-list-description " suddenly appear!"))
  on-spawn-message)
  

(define (spawn-enemies world challenge-rating)
  (define location (get-field current-location world))
  (define number challenge-rating)
  (define added-enemies '())
  (for ([i (in-range 0 number)])
    (define r (d 1 2))
    (define enemy (cond ((= r 2) (new blindscraper%))
                        (else (new bloodleech%))))

    (add-actor-to-location! location enemy)
    (set! added-enemies (cons enemy added-enemies)))
  (paragraph (get-enemies-on-spawn-message added-enemies))
  (set-field! in-combat world #t))

(define (player-has-weapons? pc)
  (not (empty? (filter
                (lambda (item) (member 'stab (send item get-uses)))
                (get-field inventory pc)))))

(define (describe-situation world)
  (define in-combat (get-field in-combat world))
  (displayln "Current location:")
  (displayln (get-field current-location world))
  (newline))

(define (describe-enemy actor index)
  (define
    description
    (string-append
     "#"
     (number->string (add1 index))
     ": "
     (send actor get-name)
     ": "
     (number->string (get-field hp actor))
     " HP."
     ))
  (displayln description))

(define (enemy? actor) ; TODO move elsewhere
  (not (is-a? actor pc%)))

(define (describe-situation-post-on-turn world)
  (cond ((get-field in-combat world)
         (define pc (get-field pc world))
         (displayln (string-append "You are in combat."
                                   " "
                                   (send pc get-status)
                                   " ("
                                   (number->string (get-field hp pc))
                                   " HP)"))
         (newline)
         (define current-location (get-field current-location world))
         (define all-actors (location-actors current-location))
         (define enemies (filter enemy? all-actors))
         (for ([i (in-range 0 (length enemies))])
           (define enemy (list-ref enemies i))
           (describe-enemy enemy i))
         (newline))))

(define (make-action-from-choice world choice)
  (define action ((choice-resolution-effect choice))) ; check actor
  action)

(define (get-world-choices world actor)
  (define current-location (get-field current-location world))
  (define location-choices
    (if (not (get-field in-combat world))
        '()#;(send (get-field current-location world)
                   get-interaction-choices)
        '()))
  (define next-location-choices
    (if (not (get-field in-combat world))
        (make-go-to-neighbor-choices current-location)
        '()))

  (define combat-choices (send actor get-combat-choices world))
  (define generic-choices (send actor get-generic-choices world))
  (displayln "find me and fix pending action choices")
  (define pending-action-choices '())
  (define all-choices (append location-choices next-location-choices combat-choices generic-choices))
  all-choices)


(provide (all-defined-out))