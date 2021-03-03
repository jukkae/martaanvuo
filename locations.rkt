#lang racket

(require "actions.rkt")
(require "items.rkt")
(require "utils.rkt")

(define location<%>
  (interface ()
    get-description
    advance-to-next-description!
    get-interactions
    get-visible-neighbors
    on-enter
    on-exit))

(define location%
  (class* object% (location<%>)
    (init-field index)
    (field [scripted-descriptions (list "TODO: First description" "TODO: Second description" "TODO: Nth description")])
    (field [neighbors '()])

    (field [biome null])
    (field [features '()])

    
    (define times-described 0)
    (define searched? #f)
    
    (super-new)

    (define/public (get-description)
      (get-procedural-description))

    (define/public (advance-to-next-description!) (set! times-described (add1 times-described)))

    (define/private (get-scripted-description) (list-ref scripted-descriptions times-described))

    (define/private (get-procedural-description)
      (define biome-description
        (cond ((eq? biome 'blackpine-forest) "You are in a blackpine forest.")
              ((eq? biome 'spruce-forest) "You are in a spruce forest.")
              ((eq? biome 'swamp) "You are in a swamp.")
              (else "unknown biome")))
      (define features-description
        (if (not (null? features))
            (cond ((eq? (car features) 'pond) " There is a small, clear-watered pond nearby. Upon closer look, it seems to be a spring, its sandy bottom shifting and bubbling.")
                  ((eq? (car features) 'big-tree) " You see an immense tree rising far above the rest. Most of its lower branches have fallen out, but its canopy spreads wide, casting a shadow over the lesser conifers.")
                  ((eq? (car features) 'spirit-rock) "There's a small clearing amidst the brush. In the middle of the clearing, there's a rather peculiarly shaped boulder.")
                  (else "unknown feature"))
            ""))
      (string-append biome-description features-description))

    (define/public (get-interactions) (if searched?
                                          null
                                          (list (make-action 'search "Search the surroundings." 3 null '(wilderness)))))

    (define/public (get-visible-neighbors)
      (define actions '())
      (map (λ (neighbor) (begin (define action (make-action 'go-to-neighboring-location
                                                            (string-append "Go to next location: " (number->string (get-field index neighbor)))
                                                            5
                                                            neighbor
                                                            (list 'wilderness)))
                                (set! actions (cons action actions)))) neighbors)
      actions)


    (define/public (on-enter)
      (displayln (string-append "Entering location " (number->string index))))
    
    (define/public (on-exit)
      (displayln (string-append "Exiting location " (number->string index))))

    (define/public (search)
      
      (define roll (d 2 6))
      (newline)
      (displayln (string-append "The area looks promising, so you take a look around." "[2d6: " (number->string roll) "]" ))
      
      (set! searched? #t)
      (define target-number 6)
      (define critical 10)
      (define loot (cond ((> roll critical) (new amulet%))
                         ((> roll target-number) (new knife%))
                         (else 'nothing)))
      loot)))

(define (make-location #:index i)
  (define location (new location% [index i]))
  (set-field! biome
              location
              (get-random-biome))

  (when (= (d 1 4) 4)
    (define number-of-additional-features 1)
    (for ([i (in-range 0 number-of-additional-features)])
      (define feature (get-random-feature))
      (set-field! features
                  location
                  (cons feature (get-field features
                                           location)))))

  location)

(define (get-random-biome)
  (define biomes (list 'blackpine-forest 'swamp 'spruce-forest))
  (take-random biomes))

(define (get-random-feature)
  (define features (list 'pond 'big-tree 'spirit-rock))
  (take-random features))

(provide (all-defined-out))
