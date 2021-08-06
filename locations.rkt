#lang racket

(provide (all-defined-out))

(require racket/lazy-require)

(require "action.rkt")
(require "io.rkt")
(require "location.rkt")
(require "route.rkt")

(lazy-require ["situation.rkt"
               (current-location)])


(displayln "TODO: move times-narrated to situation")
(define *times-begin-traverse-narrated* (make-hash)) ; per each pair
(define *times-finish-traverse-narrated* (make-hash)) ; per each pair
(define *times-cancel-traverse-narrated* (make-hash)) ; essentially for each route, so both endpoints have to be tracked

(define (describe-begin-traverse-action action)
  (define from
    (cond ((route? (action-target action))
           (if (memq 'a-to-b (action-details action))
               (route-a (action-target action))
               (route-b (action-target action))))
          (else
           (current-location))
          ))

  (define to
    (cond ((route? (action-target action))
           (if (memq 'a-to-b (action-details action))
               (route-b (action-target action))
               (route-a (action-target action))))
          (else
           (action-target action))
          ))

  (define key (list from to))
  (when (not (hash-has-key? *times-begin-traverse-narrated* key))
    (hash-set! *times-begin-traverse-narrated* key 0))
  (hash-set! *times-begin-traverse-narrated* key (add1 (hash-ref *times-begin-traverse-narrated* key)))
  (define n (hash-ref *times-begin-traverse-narrated* key))
  (case (location-id from)
    ['perimeter
     (case (location-id to)
       ['magpie-hill
        (case n
          [(1)
           (paragraph
            "Drawn by the magpie's call, Otava begins her ascent. The trail turns into a narrow, natural staircase of rocks, as the hillside steepens to a cliff.")]
          [else
           (paragraph
            "Otava climbs the natural stairs up to Magpie Hill.")])
     
        ])

     
     ]))

(define (describe-finish-traverse-action action)
  (define from
    (cond ((route? (action-target action))
           (if (memq 'a-to-b (action-details action))
               (route-a (action-target action))
               (route-b (action-target action))))
          (else
           (current-location))
          ))

  (define to
    (cond ((route? (action-target action))
           (if (memq 'a-to-b (action-details action))
               (route-b (action-target action))
               (route-a (action-target action))))
          (else
           (action-target action))
          ))

  (define key (list from to))
  (when (not (hash-has-key? *times-finish-traverse-narrated* key))
    (hash-set! *times-finish-traverse-narrated* key 0))
  (hash-set! *times-finish-traverse-narrated* key (add1 (hash-ref *times-finish-traverse-narrated* key)))
  (define n (hash-ref *times-finish-traverse-narrated* key))
  (case (location-id from)
    ['perimeter
     (case (location-id to)
       ['magpie-hill
        (case n
          [(1)
           (paragraph "Otava comes to a slab of stone with a painting of a human figure on it. The being has the head of an ant, and its six arms are contorted in a mad dance, one pair of hands gesturing wildly with the control rods of a puppeteer. The bottom of the painting is washed away. The figure's unsettling gaze follows Otava as she goes past the painting and fills her with apprehension. It begins to drizzle.")
           (paragraph "Soon after the dreadful painting, the rocky stairs turn back to a trail that levels out. The thick fog and the drizzle that's now a steady rain obscure much of the view. Otava is at the edge of a large plateau. The silhouette of a decaying industrial building looms in the distance. Is this it? The Facility?")]
          [else
           (paragraph
            "The dreadful painting of Anthead God gazes at Otava mockingly as she passes the painted stone slab.")])
     
        ])

     
     ]))
  


(define (describe-perimeter-cancel-traverse-to-action)
  (paragraph "Otava comes back to the fork in the path somewhere in Perimeter."))

(define (describe-cancel-traverse-action action)
  (define from
    (cond ((route? (action-target action))
           (if (memq 'a-to-b (action-details action))
               (route-a (action-target action))
               (route-b (action-target action))))
          (else
           (current-location))
          ))

  (define to
    (cond ((route? (action-target action))
           (if (memq 'a-to-b (action-details action))
               (route-b (action-target action))
               (route-a (action-target action))))
          (else
           (action-target action))
          ))

  (define key (list from to))
  (when (not (hash-has-key? *times-cancel-traverse-narrated* key))
    (hash-set! *times-cancel-traverse-narrated* key 0))
  (hash-set! *times-cancel-traverse-narrated* key (add1 (hash-ref *times-cancel-traverse-narrated* key)))
  (define n (hash-ref *times-cancel-traverse-narrated* key))
  (case (location-id from)
    ['perimeter
     (case (location-id to)
       ['magpie-hill
        (case n
          [(1)
           (paragraph "Otava comes back to the fork in the path somewhere in Perimeter.")]
          [else
           (paragraph "Otava comes back to the fork in Perimeter.")])
     
        ])

     
     ]))


; This seems to be more content than code, so it's here for now, instead of location.rkt
(define (location-on-enter! location)
  (displayln "LOCATION-ON-ENTER")
  (displayln location))