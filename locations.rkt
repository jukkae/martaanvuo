#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "action.rkt")
(require "io.rkt")
(require "location.rkt")
(require "route.rkt")

(lazy-require ["situation.rkt"
               (current-location)])

(define (describe-begin-go-to-action action)
  (define from (current-location))

  (define to
    (cond ((route? (action-target action))
           (route-b (action-target action))) ; TODO FIX DIRECTION
          (else
           (displayln "HELLO FIND ME")
           (displayln (action-target action))
           (action-target action))
          ))
  (cond ((location-is? 'magpie-hill to)
         (describe-magpie-hill-begin-go-to-action))
        (else
         (paragraph
          "[[begin-go-to description not written yet]"))))

; shit names for shit functions /shrug
(define *magpie-hill-begin-go-to-nth* 0)
(define (describe-magpie-hill-begin-go-to-action)
  (set! *magpie-hill-begin-go-to-nth* (add1 *magpie-hill-begin-go-to-nth*))
  (define n *magpie-hill-begin-go-to-nth*)
  (case n
    [(1)
     (paragraph
      "Drawn by the magpie's call, Otava begins her ascent. The trail turns into a narrow, natural staircase of rocks, as the hillside steepens to a cliff.")]
    [else
     (paragraph
      "Otava climbs the natural stairs up to Magpie Hill.")]))

(define *magpie-hill-finish-go-to-nth* 0)
(define (describe-magpie-hill-finish-go-to-action)
  (set! *magpie-hill-finish-go-to-nth* (add1 *magpie-hill-finish-go-to-nth*))
  (define n *magpie-hill-finish-go-to-nth*)
  (case n
    [(1)
     (paragraph "Otava comes to a slab of stone with a painting of a human figure on it. The being has the head of an ant, and its six arms are contorted in a mad dance, one pair of hands gesturing wildly with the control rods of a puppeteer. The bottom of the painting is washed away. The figure's unsettling gaze follows Otava as she goes past the painting and fills her with apprehension. It begins to drizzle.")
     (paragraph "Soon after the dreadful painting, the rocky stairs turn back to a trail that levels out. The thick fog and the drizzle that's now a steady rain obscure much of the view. Otava is at the edge of a large plateau. The silhouette of a decaying industrial building looms in the distance. Is this it? The Facility?")]
    [else
     (paragraph
      "The dreadful painting of Anthead God gazes at Otava mockingly as she passes the painted stone slab.")]))
  


(define (describe-finish-go-to-action action)
  (define from (current-location))

  (define to
    (cond ((route? (action-target action))
           (route-b (action-target action))) ; TODO FIX DIRECTION
          (else
           (displayln "HELLO FIND ME")
           (displayln (action-target action))
           (action-target action))
          ))
  (cond ((location-is? 'magpie-hill to)
         (describe-magpie-hill-finish-go-to-action))
        (else
         (paragraph
          "[[finish-go-to description not written yet]"))))
