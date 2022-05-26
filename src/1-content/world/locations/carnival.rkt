#lang at-exp racket

(provide (all-defined-out))

(require
  "../../../0-engine/0-api/api.rkt")

(require
  "../../../0-engine/3-types/choice.rkt"
  )

(fragment
 'bobo-the-clown-1
 (thunk
  (p @~a{
Bobo the Clown waves his arms, squeezes his honking nose, breathes in from a balloon, and shouts in a voice turned chipmunk-like by the helium.

BOBO THE CLOWN:
Hey you! Yes, you! Come here! Come, huff a puff of Bobo's stuff! Have some gas – some laughing gas! It's – the gas – some two dollars, laughing HA HA HA new person.

Bobo the Clown puts a mask on his mouth, and there's a click and a TSCHHHHHHHHHHHhhhhh as gas begins to flow. Bobo the Clown inhales and continues.

BOBO THE CLOWN:
Two gas laughing new person ha ha ha you want? It will make your miiiiiiiiiiiiind ha haha your mind you'll think somebody yes new huh ha ha ha? Yes? Yeees?
 }))
 #:decisions
 (list (make-decision
        #:requirement (λ () (pc-has-money 2))
        #:title "Yeeees."
        #:next-fragment 'exit
        )
       (make-decision
        #:requirement (λ () (pc-has-money 2))
        #:title "No."
        #:next-fragment 'exit
        )
       (make-decision
        #:requirement (λ () (not (pc-has-money 2)))
        #:title "Can't afford it."
        #:next-fragment 'exit
        )))

; alternate selves: Madam Sibyl, Adam Sybyl, Mad Sibylla, etc.
(fragment
 'fortune-teller
 (thunk
  (p @~a{
Madame Sybil's tent is small and cramped, full of heavy-hanging plush purple velvet drapery. It is dimly lit by two candles on a table, behind which a fat old woman sits, wearing a plush purple velvet robe, thick-rimmed glasses and heavy, glittering jewelry. There's a plush purple velvet pouf, upon which she directs Otava to sit.

MADAME SYBIL:
Come and sit, child! Let Madame Sybil read your palm and tell you about your life lines! Or perhaps you'd like to ask the worms guidance. Or I can view you through the Sacred Orb of Seeing, and tell you your One True Purpose in Life!

Level one palm reading costs two dollars, worms are three dollars, and the Sacred Orb of Seeing costs 52 dollars.
  }))
 #:decisions
 (list (make-decision
        #:title "Level one palm reading. [-2 dollars]"
        #:next-fragment 'ft-standard-divination
        #:requirement (λ () (pc-has-money 2))
        )
       (make-decision
        #:title "Worms' guidance. [-3 dollars]"
        #:next-fragment 'ft-worms
        #:requirement (λ () (pc-has-money 3))
        )
       (make-decision
        #:title "Sacred Orb of Seeing. [-52 dollars]"
        #:next-fragment 'ft-sacred-orb-of-seeing
        #:requirement (λ () (pc-has-money 52))
        )
       (make-decision
        #:title "Nothing, thanks. [exit]"
        #:next-fragment 'exit
        #:requirement (λ () (pc-has-money 2))
        )
       (make-decision
        #:title "Can't afford anything. [exit]"
        #:next-fragment 'exit
        #:requirement (λ () (not (pc-has-money 2)))
        )
       ))

(fragment
 'ft-standard-divination
 (thunk
  (p @~a{
Madame Sybil takes Otava's right hand between her soft, pale fingers, and begins studying the patterns.

MADAME SYBIL:
Hm. Hm hm hm. Mmh.

She takes a closer look and sighs.

OTAVA:
What?

MADAME SYBIL:
I am afraid I don't have many good news. See, your heart line here, it can represent many things. Many think it is about love, but I am more concerned with what it tells in conjunction with, or dare I say *in opposition to*, your head line.

Madame Sybil pokes Otava's palm.

MADAME SYBIL:
See, the two lines are fractured, in pieces, and almost at war with each other. This means that you will never find lasting peace. You will be forced to make painful sacrifices. Your head line – representing your mind – well, it is of decent length, meaning a broad range of interests and capabilities, but see here for instance, your head line is completely fragmented, in pieces.

OTAVA:
What does that mean?

MADAME SYBIL:
Well, to be completely frank, I have never encountered a head line quite this fragmented. Sometimes they have been great artists or thinkers, or powerful people, but not always. Yours... It is an exceptional head line, in any case. And there's another thing.

OTAVA:
What other thing?

MADAME SYBIL:
Your life line. It's extremely weak, and there's a clear cut in it. I am afraid that this does not bode you well, my child. And here, see these swirling loops before the cut? If what I suspect is true, then not only will you encounter death, you will suffer along the way. Now, that'll be two dollars.
  })
  (decrease-pc-money! 2)
  (award-xp! 4))
 #:decisions
 (list (make-decision
        #:title "Exit the tent."
        #:next-fragment 'exit
        )))

; The carnival could be randomly generated, so you have to wander around until events are drawn from a deck
(define (get-carnival-choices)
  (define current-day (add1 (exact-floor (/ (world-elapsed-time (current-world)) day-length)))) ; TODO: ACCESSOR!!!
  (cond [(even? current-day)
      (list
    (make-choice
      'bobo-the-clown
      "Bobo the Clown."
      (λ ()
        (go-to-fragment 'bobo-the-clown-1)
        'end-chapter ; ie., 'end-round-early, plus next chapter on next round
        ))
    (make-choice
      'the-endless-staircase
      "The Endless Staircase."
      (λ ()
        (go-to-fragment 'endless-staircase-1)
        'end-chapter ; ie., 'end-round-early, plus next chapter on next round
        ))
    (make-choice
      'hall-of-mirrors
      "Hall of Mirrors"
      (λ ()
        (go-to-fragment 'hall-of-mirrors-1)
        'end-chapter ; ie., 'end-round-early, plus next chapter on next round
        ))
    (make-choice
      'emporium-of-oddities
      "The Merchant's Bio-Mechanical Emporium of Oddities."
      (λ ()
        (go-to-fragment 'the-other-merchant-1)
        'end-chapter ; ie., 'end-round-early, plus next chapter on next round
        ))
    (make-choice
      'fortune-teller
      "Fortune teller"
      (λ ()
        (go-to-fragment 'fortune-teller)
        'end-chapter ; ie., 'end-round-early, plus next chapter on next round
        ))
  )
    ]
    [else
     '()
    ])
  )

(define (get-carnival-forbidden-actions)
  (list 'rest))