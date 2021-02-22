#lang racket

(require "actions.rkt")
(require "items.rkt")
(require "utils.rkt")

; locations

(define location<%> (interface () get-description get-interactions get-visible-exits))
(define forest%
  (class* object% (location<%>)
    (define times-described 0)
    (define searched? #f)
    (super-new)

    (define/public (get-nth-description n)
      (when (< times-described n) (set! times-described n))
      (cond ((= n 1) "You are walking through a dense coniferous forest. It is bitterly cold.")
            ((= n 2) (string-append "The Sun has come up a while ago. Her pale light scarcely filters through the branches of age-old pines. "
                                    "You hear the babbling of a small CREEK flowing on your right. "
                                    "The icy hill is steep, almost a cliff, and the rocks at the bottom look much harder and sharper than your head."))
            ((= n 3) "You are walking along a narrow path, too narrow to be human. The babbling of the creek is quieting down, but the downhill looks rather doable.")
            ((= n 4) "You are walking along a forest path. The mountains are drawing nearer.")
            ((= n 5) "You are on the foothills of the mountains.")
            (else "You are utterly lost in the Dead Woods. Or was it the Woods of Dead? The woods seem very much alive to you.")))
      
    (define/public (get-description)
      (begin (set! times-described (add1 times-described))
             (get-nth-description times-described)))

    (define/public (get-interactions) (if searched?
                                          null
                                          (list (make-action 'search "Search the surroundings." 3 null '(wilderness)))))

    (define/public (get-visible-exits)
      (define n times-described)
      (cond ((= n 1) (list (make-action 'go-on "Go deeper into the forest." 1 null '(wilderness))))
            ((= n 2) (list (make-action 'go-on "Go deeper into the forest." 1 null '(wilderness))
                           (make-action 'go-to-river "Try to get to the river." 1 null '(wilderness))))
            ((= n 3) (list (make-action 'go-on "Go deeper into the forest." 1 null '(wilderness))
                           (make-action 'go-to-river "Try to get to the river." 1 null '(wilderness))))
            ((= n 4) (list (make-action 'go-on "Follow the path." 1 null '(wilderness))))
            ((= n 5) (list (make-action 'go-to-mountains "Climb the mountains." 1 null '(wilderness))))
            (else '())))

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
      (cond ((= n 1) "You are on the foothills of the mountains.")
            (else "The mountains are impressive.")))

    (define/public (get-description)
      (begin (set! times-described (add1 times-described))
             (get-nth-description times-described)))

    (define/public (get-interactions) (if searched?
                                          null
                                          (list (make-action 'search "Search the surroundings." 3 null '(wilderness)))))
    (define/public (get-visible-exits)
      (list (make-action 'go-on "Climb towards the summit." 1 null '(wilderness))))

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
      (cond ((= n 1) "You fall down the slope")
            (else "Bruised but alive, you come to the river. Its water is refreshing.")))

    (define/public (get-description)
      (begin (set! times-described (add1 times-described))
             (get-nth-description times-described)))

    (define/public (get-interactions) (if searched?
                                          null
                                          (list (make-action 'search "Search the surroundings." 3 null '(wilderness)))))
    (define/public (get-visible-exits)
      (list (make-action 'go-to-forest "Try to get back to the path. It has the marks of the Spiritdrinker and I need his power." 1 null '(wilderness))
            (make-action 'go-downriver "Go downriver." 1 null '(wilderness))))

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