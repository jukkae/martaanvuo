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
            ((= n 2) "The Sun has come up a while ago. Her pale light scarcely filters through the branches of age-old pines.")
            ((= n 3) "You realise you are walking along a narrow path, too narrow to be human.")
            ((= n 4) "You are walking along a forest path.")
            ((= n 5) "You are walking along a forest path. You hear a RIVER to your right.")
            ((= n 6) "You are walking along a forest path. You hear a river to your right.")
            (else "You are utterly lost in the Dead Woods. Or was it the Woods of Dead? The woods seem very much alive to you.")))
      
    (define/public (get-description)
      (begin (set! times-described (add1 times-described))
             (get-nth-description times-described)))

    (define/public (get-interactions) (if searched?
                                          null
                                          (list (make-action 'search "Search the surroundings." 3 null '(wilderness)))))

    (define/public (get-visible-exits)
      (if (< times-described 4)
          (list (make-action 'go-on "Go deeper into the forest." 1 null '(wilderness)))
          (list (make-action 'go-on "Follow the path." 1 null '(wilderness))
                (make-action 'go-to-mountains "Climb the mountains." 1 null '(wilderness)))))

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

(provide (all-defined-out))