#lang at-exp racket

(provide (all-defined-out))

(require
  "../../0-engine/0-api/api.rkt"
  )

; See scenario.rkt for map overview
(define places
  (list
   (place
    #:id 'perimeter
    #:size 'large
    #:type 'ext
    #:encounter-types '(voidfloater)
    #:hidden-features (list 'nothing)
    #:shortname "perimeter")

   (place
    #:id 'magpie-hill
    #:size 'large
    #:features '()
    #:hidden-features (list 'cocoon-effigy 'nothing)
    #:type 'ext
    #:shortname "Magpie hill")

  (place
    #:id 'lookout
    #:features '()
    #:type 'int)

   (place
    #:id 'shack
    #:features '()
    #:items (list (make-item 'revolver))
    #:type 'int)

   (place
    #:id 'pond-of-drowning
    #:features '()
    #:items (list (make-item 'gold #:amount 7) (make-item 'human-remains))
    #:type 'ext
    #:shortname "pond")


   (place
    #:id 'martaanvuo-dam
    #:features '()
    #:type 'ext
    #:shortname "Martaanvuo dam")

   (place
    #:id 'martaanvuo-river
    #:features '()
    #:type 'ext
    #:shortname "Martaanvuo river")

   (place
    #:id 'village
    #:choices
    (list
     (make-choice
      'visit-forge
      "Visit the village forge"
      `(
        (go-to-fragment 'seppo)
        '()
        ))
     (make-choice
      'visit-shaman
      "Visit the village shaman"
      `(
        (go-to-fragment 'shaman)
        '()
        ))
    )
    #:type 'ext
    #:shortname "Village")


   (place
    #:id 'the-maw
    #:type 'int
    #:size 'small
    #:tags '(cluttered)
    #:shortname "The Maw"
    #:encounter-types '(limbtearer voidfloater blindscraper two-blindscrapers)
    #:light-level 'dark
    #:features (list 'light-switch 'martaanvuo-terminal)
    )

  (place
    #:id 'reactor-room
    #:type 'int
    #:size 'small
    #:tags '(cluttered)
    #:shortname "reactor room"
    #:encounter-types '(limbtearer voidfloater blindscraper two-blindscrapers)
    #:light-level 'dark
    #:features (list 'the-button 'running-centrifuge)
    )

  (place
    #:id 'bioreactor
    #:type 'int
    #:size 'container
    #:tags '()
    #:shortname "bioreactor"
    #:encounter-types '(limbtearer voidfloater blindscraper two-blindscrapers)
    #:light-level 'dark
    #:features (list 'the-button)
    )

  ;  (place
  ;   #:id 'waiting-room
  ;   #:type 'int
  ;   #:features '(waiting-room-begin))

  ;  (place
  ;   #:id 'carnival
  ;   #:type 'ext
  ;   #:features '(the-endless-staircase bobo-the-clown the-merchant fortune-teller)
  ))

(define routes
  (list
    (route-between 'perimeter 'magpie-hill 130 'ext #:hidden? #t #:descr-from-a "steep, rocky ascent" #:descr-from-b "steep, rocky descent")
    (route-between 'magpie-hill 'lookout 10 'ext #:hidden? #f)
    (route-between 'magpie-hill 'shack 40 'ext #:hidden? #t)
    (route-between 'magpie-hill 'pond-of-drowning 30 'ext #:hidden? #t)

    (route-between 'perimeter 'martaanvuo-dam 140 'ext #:hidden? #t #:descr-from-a "winding path downhill" #:descr-from-b "winding path uphill")
    (route-between 'martaanvuo-dam 'martaanvuo-river 40 'ext)
    (route-between 'martaanvuo-river 'village 140 'ext)

    (route-between 'martaanvuo-dam 'the-maw 10 'ext #:hidden? #t)
    (route-between 'the-maw 'reactor-room 1 'int)
    (route-between 'reactor-room 'bioreactor 1 'int #:details (list 'only-when-small))
  ))

(provide make-new-world)
(define (make-new-world)
  (connect-places-and-routes! places routes)
  (world places routes 0 0))
