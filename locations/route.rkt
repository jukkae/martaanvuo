#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "location.rkt")

(lazy-require
 ["place.rkt"
  (place-id
   place-shortname
   place-visited?)])

(lazy-require
 ["state/state.rkt"
  (get-pending-traverse-direction)])

(serializable-struct
 route
 location
 ([a #:mutable]
  [b #:mutable])
 #:transparent
 #:constructor-name route*)

(define
  (make-route id
              a
              b
              #:details [details '()]
              #:actors [actors '()]
              #:features [features '()])

  (route* id
          a
          b
          details
          actors
          features))



(define (get-traverse-text route start-location)
  (define direction
    (cond ((eq? (place-id start-location)
                (place-id (route-a route)))
           'a-to-b)
          ((eq? (place-id start-location)
                (place-id (route-b route)))
           'b-to-a)))

  (define (get-route-short-description)

    (case (location-id start-location)

      ['perimeter
       (case (location-id (route-other-end-from route start-location))

         ['magpie-hill
          (if (route-fully-known? route)
              "Rocky stairs up Magpie Hill."
              "Magpie and the rocky slope.")]
         
         ['martaanvuo-swamp
          (if (route-fully-known? route)
              "Martaanvuo Swamp."
              "Ants and the swamp.")]
         [else (string-append "["
                              "go to: "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]
      
      ['magpie-hill
       (case (location-id (route-other-end-from route start-location))
         ['perimeter
          (if (route-fully-known? route)
              "Rocky stairs to Perimeter"
              "The steep descent.")]
         ['martaanvuo-swamp
          (if (route-fully-known? route)
              "The path to the fork in Martaanvuo Swamp."
              "The downhill path.")]
         ['power-plant-ruins
          (if (route-fully-known? route)
              "The power plant ruins."
              "The decrepit building.")]
         [else (string-append "["
                              "go to: "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]
      
      ['martaanvuo-swamp
       (case (location-id (route-other-end-from route start-location))
         ['magpie-hill
          (if (route-fully-known? route)
              "Magpie Hill."
              "The path uphill and to the left.")]
         ['perimeter
          (if (route-fully-known? route)
              "Go back to Perimeter."
              "The lone path away from the fork.")]
         ['crematory
          (if (route-fully-known? route)
              "Crematory."
              "The small side path to the right.")]
         ['martaanvuo-docks
          (if (route-fully-known? route)
              "The Docks."
              "The broader path straight ahead.")]
         [else (string-append "["
                              "go to: "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]

      ['crematory
       (case (location-id (route-other-end-from route start-location))
         ['martaanvuo-swamp
          (if (route-fully-known? route)
              "Back to Martaanvuo Swamp."
              "The lone path.")]
         [else (string-append "["
                              "go to: "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]

      ['martaanvuo-docks
       (case (location-id (route-other-end-from route start-location))
         ['martaanvuo-swamp
          (if (route-fully-known? route)
              "Back to Martaanvuo Swamp."
              "The vehicle trail out.")]
         [else (string-append "["
                              "go to: "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]
      
      ['power-plant-ruins
       (case (location-id (route-other-end-from route start-location))
         ['cache "The previously locked door."]
         ['sewers-1 "The sewers."]
         [else (string-append "["
                              "go to: "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]

      [else (string-append "["
                           "go to: "
                           (symbol->string (location-id (route-other-end-from route start-location)))
                           "]")]))
  

  
  #;(cond ((route-fully-known? route)
           ; Currently, "fully known" implies having been at the other end, fix as needed
           (case direction
             ['a-to-b
              (define to-name (get-location-name-from-location (route-b route)))
              (string-append "Go back to " to-name ".") ; breaks eg. when recursing into a new run
              ]
             ['b-to-a
              (define to-name (get-location-name-from-location (route-a route)))
              (string-append "Go back to " to-name ".") ; breaks eg. when recursing into a new run
              ]))
          (else
           (get-route-short-description)))
  (get-route-short-description))

(define (route-other-end-from route start-location)
  (define start
    (cond ((eq? (place-id start-location)
                (place-id (route-a route)))
           'a)
          ((eq? (place-id start-location)
                (place-id (route-b route)))
           'b)))
  (define endpoint
    (case start
      ['a (route-b route)]
      ['b (route-a route)]))
  endpoint)



(define (set-route-endpoint-visited! route location)
  (define endpoint
    (cond ((eq? (place-id location)
                (place-id (route-a route)))
           'a)
          ((eq? (place-id location)
                (place-id (route-b route)))
           'b)))
  (case endpoint
    ['a (add-detail-to-location! route 'a-visited)]
    ['b (add-detail-to-location! route 'b-visited)]))

(define (route-traversed? route)
  (location-has-detail? route 'traversed))

(define (route-a-visited? route)
  (location-has-detail? route 'a-visited))

(define (route-b-visited? route)
  (location-has-detail? route 'b-visited))

(define (route-place-known? route place)
  (define endpoint
    (cond ((eq? (place-id place)
                (place-id (route-a route)))
           'a)
          ((eq? (place-id place)
                (place-id (route-b route)))
           'b)))
  (case endpoint
    ['a (place-visited? (route-a route))]
    ['b (place-visited? (route-b route))])
  )

(define (set-route-traversed! route)
  (add-detail-to-location! route 'traversed))

(define (route-fully-known? route)
  (and (route-a-visited? route)
       (route-b-visited? route)
       (route-traversed? route)))



(define (route-shortname route)
  (define direction (get-pending-traverse-direction))

  (define startpoint
    (case direction
      ['a-to-b (route-a route)]
      ['b-to-a (route-b route)]))
  (define endpoint
    (case direction
      ['a-to-b (route-b route)]
      ['b-to-a (route-a route)]))

  
  
  (cond ((route-fully-known? route)
         (string-append "En route: "
                        (place-shortname startpoint)
                        " – "
                        (place-shortname endpoint)
                        " "))
        (string-append "En route: "
                       (place-shortname startpoint)
                       " – "
                       "???"
                       " ")))
