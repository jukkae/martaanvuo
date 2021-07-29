#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "action.rkt")
(require "io.rkt")
(require "location.rkt")

(lazy-require ["situation.rkt"
               (current-location)])

; store in the action, handle calling from here
; -> code to action handler?
(define (describe-begin-go-to-action action)
  (define from (current-location))
  (define to (action-target action))
  (cond ((location-is? 'magpie-hill to)
         (paragraph
          "Drawn by the magpie's call, Otava begins her ascent. The trail turns into a narrow, natural staircase of rocks, as the hillside steepens to a cliff."))
        (else
         (paragraph
          "[[begin-go-to description not written yet]"))))

; store in the action, handle calling from here
; -> code to action handler?
(define (describe-finish-go-to-action action)
  (define from (current-location))
  (define to (action-target action))
  (cond ((location-is? 'magpie-hill to)
         (paragraph "Otava comes to a slab of stone with a painting of a human figure on it. The being has the head of an ant, and its six arms are contorted in a mad dance, one pair of hands gesturing wildly with the control rods of a puppeteer. The bottom of the painting is washed away. The figure's unsettling gaze follows Otava as she goes past the painting and fills her with apprehension. It begins to drizzle.")
         (paragraph "Soon after the dreadful painting, the rocky stairs turn back to a trail that levels out. The thick fog and the drizzle that's now a steady rain obscure much of the view. Otava is at the edge of a large plateau. The silhouette of a decaying industrial building looms in the distance. Is this it? The Facility?"))
        (else
         (paragraph
          "[[finish-go-to description not written yet]"))))
