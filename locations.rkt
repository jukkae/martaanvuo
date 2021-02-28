#lang racket

(require "commands.rkt")
(require "items.rkt")
(require "utils.rkt")

; Locations and routes should form a graph, so that locations may have arbitrarily
; many neighbors of each kind, whereas routes have 1 or 2 neighbors in total.
; This is to make it easier to write good prose for routes, as well as providing
; easier-to-understand structure to the narrative, plus some mechanical possibilities.
; Notably, this does require representing branches and junctions as locations.
; This is not necessarily a bad thing.
; Also, routes should be asymmetrical as often as possible, to make the gameplay
; more interesting!
; Basically, routes should have a chance of having a route event, beneficial or
; not, at least the first time the route is traversed. Subsequent events depend on
; situation.
; PC is either at the beginning of the route, in the middle of one, or at the end of it.
; Check happens when PC enters middle of one. PC should sometimes (not always) have a
; choice of pushing on or turning back.
(define location<%>
  (interface ()
    get-description
    advance-to-next-description!
    get-interactions
    get-visible-exits))

(define forest%
  (class* object% (location<%>)
    (define times-described 0)
    (define searched? #f)
    (super-new)

    (define/public (get-nth-description n)
      (when (< times-described n) (set! times-described n))
      (cond ((= n 0) "You are walking through a dense Blackpine forest. It is bitterly cold still. Spring is far overdue.")
            ((= n 1) (string-append "The Sun has come up a while ago. Her pale light scarcely filters through the branches of age-old pines. "
                                    "You hear the babbling of a small CREEK flowing on your right, at the bottom of a frozen, rocky hill. "
                                    "The icy hill is steep, almost a cliff, and the rocks at the bottom look much harder and sharper than your head."))
            ((= n 2) "You are walking along a narrow path, too narrow to be human. The babbling of the creek is quieting down, but the downhill looks rather doable.")
            ((= n 3) "You are walking along a forest path. The mountains are drawing nearer.")
            ((= n 4) "You are on the foothills of the mountains.")
            (else "You are utterly lost in the Dead Woods. Or was it the Woods of Dead? The woods seem very much alive to you.")))
      
    (define/public (get-description) (get-nth-description times-described))

    (define/public (advance-to-next-description!) (set! times-described (add1 times-described)))

    (define/public (get-interactions) '())

    (define/public (get-visible-exits)
      (define n times-described)
      '())

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
      loot)
    (define/public (camp)
      (displayln "---"))))

(define mountains%
  (class* object% (location<%>)
    (define times-described 0)
    (define searched? #f)
    (super-new)

    (define/public (get-nth-description n)
      (cond ((= n 0) "You are on the foothills of the mountains.")
            (else "The mountains are impressive.")))

    (define/public (get-description) (get-nth-description times-described))

    (define/public (advance-to-next-description!) (set! times-described (add1 times-described)))

    (define/public (get-interactions) '())
    (define/public (get-visible-exits) '())

    (define/public (search)
      
      (define roll (d 2 6))
      (newline)
      (displayln (string-append "The area looks promising, so you take a look around." "[2d6: " (number->string roll) "]" ))
      
      #;(set! searched? #t)
      (define target-number 6)
      (define critical 10)
      (define loot (cond ((> roll critical) (new figurine%))
                         (else 'nothing)))
      loot)
    (define/public (camp)
      (displayln "---"))))

(define river%
  (class* object% (location<%>)
    (define times-described 0)
    (define searched? #f)
    (super-new)

    (define/public (get-nth-description n)
      (cond ((= n 0) "You fall down the slope")
            (else "Bruised but alive, you come to the river. Its water is refreshing.")))

    (define/public (get-description) (get-nth-description times-described))

    (define/public (advance-to-next-description!) (set! times-described (add1 times-described)))

    (define/public (get-interactions) '())
    (define/public (get-visible-exits)
      '())

    (define/public (search)
      
      (define roll (d 2 6))
      (newline)
      (displayln (string-append "The area looks promising, so you take a look around." "[2d6: " (number->string roll) "]" ))
      
      #;(set! searched? #t)
      (define target-number 6)
      (define critical 10)
      (define loot (cond ((> roll critical) (new figurine%))
                         (else 'nothing)))
      loot)
    (define/public (camp)
      (displayln "---"))))

(provide (all-defined-out))