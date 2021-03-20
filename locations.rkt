#lang racket

(require "actions.rkt")
(require "items.rkt")
(require "utils.rkt")

(define location<%>
  (interface ()
    get-description
    get-combat-summary
    advance-to-next-description!
    get-interaction-choices
    get-exit-choices
    on-enter!
    on-exit!
    add-actor!))

(define location%
  (class* object% (location<%>)
    (init-field index)
    (field [scripted-descriptions (list "TODO: First description" "TODO: Second description" "TODO: Nth description")])
    (field [neighbors '()])

    (field [topography null])
    (field [biome null])
    (field [features '()])

    (field [actors '()])

    
    (define times-described 0)
    (define searched? #f)
    
    (super-new)

    (define/public (get-description) (string-append (symbol->string biome) ", " (symbol->string topography)))

    (define/public (get-combat-summary)
      "TODO: Location combat summary")

    (define/public (advance-to-next-description!) (set! times-described (add1 times-described)))

    (define/private (get-scripted-description) (list-ref scripted-descriptions times-described))

    (define/public (get-interaction-choices)
      '())

    (define/private (make-go-to-neighbor-choice neighbor index)

      (define direction-string
        (case index
          [(0) "Go east"]
          [(1) "Go northeast"]
          [(2) "Go southeast"]
          [else (error "locations.rkt: direction-string: Unknown direction")]))
      ; (define biomes (list 'blackpine-forest 'swamp 'spruce-forest 'larch-forest 'scrub 'sparse 'dead-vegetation 'barren))
      (define (get-biome-string)
        (define biome (get-field biome neighbor))
        (case biome
          ['blackpine-forest "towards a blackpine forest"]
          ['swamp "towards a swamp"]
          ['spruce-forest "towards a thicket of spruces"]
          ['larch-forest "towards a larch forest"]
          ['scrub "towards scrub"]
          ['sparse "towards sparser vegetation"]
          ['dead-vegetation "towards dead vegetation"]
          ['barren "towards barren land"]
          [else (error "locations.rkt: biome-string: Unknown biome")]))
      (make-choice 'go-to-neighboring-location
                   (string-append direction-string
                                  ", "
                                  (get-biome-string)
                                  " (location index "
                                  (number->string (get-field index neighbor))
                                  ")")
                   (λ () (make-action #:symbol 'go-to-neighboring-location
                                      #:actor 'pc
                                      #:duration (* (+ (d 1 6) 7) 10) ; 7+1d6 -> range of 80-130 jiffies
                                      #:target neighbor
                                      #:tags '(wilderness)))))

    (define/public (get-exit-choices)
      (define choices '())
      (for ([i (in-range 0 (length neighbors))])
        (define neighbor (list-ref neighbors i))
        (define choice (make-go-to-neighbor-choice neighbor i))
        (set! choices (cons choice choices)))
        
      choices)


    (define/public (on-enter!)
      (displayln (string-append "Entering location " (number->string index))))
    
    (define/public (on-exit!)
      (displayln (string-append "Exiting location " (number->string index))))

    (define/public (add-actor! actor)
      (define new-actors (cons actor actors))
      (set! actors new-actors))

    (define/public (remove-actor! actor)
      (define new-actors (remove actor actors))
      (set! actors new-actors))

    (define/public (search)
      
      (define roll (d 2 6))
      (newline)
      (displayln (string-append "The area looks promising, so you take a look around." "[2d6: " (number->string roll) "]" ))
      
      (set! searched? #t)
      (define target-number 6)
      (define critical 10)
      
      '())))

(define (make-location #:index i)
  (define location (new location% [index i]))

  (define topography (take-random topographies))
  (set-field! topography
              location
              topography)


  (set-field! biome
              location
              (get-random-biome topography))
  

  (when (= (d 1 4) 4)
    (define number-of-additional-features 1)
    (for ([i (in-range 0 number-of-additional-features)])
      (define feature (take-random features))
      (set-field! features
                  location
                  (cons feature (get-field features
                                           location)))))

  location)

(define topographies (list 'flat 'cragged 'highlands))
(define biomes (list 'blackpine-forest 'swamp 'spruce-forest 'larch-forest 'scrub 'sparse 'dead-vegetation 'barren))
(define features (list 'pond 'big-tree 'spirit-rock))

(define (get-random-biome topography)
  (case topography
    [(flat) (take-random (list 'swamp 'spruce-forest 'larch-forest 'scrub 'sparse))]
    [(cragged) (take-random (list 'spruce-forest 'blackpine-forest 'scrub 'sparse 'dead-vegetation 'barren))]
    [(highlands) (take-random (list 'sparse 'dead-vegetation 'barren))]
    [(shore) (take-random (list 'swamp 'larch-forest))]
    [else (error "locations.rkt: get-random-biome: unknown topography!")]))

(provide (all-defined-out))
