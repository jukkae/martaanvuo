#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "../io.rkt")
(require "../utils.rkt")

(lazy-require
 ["location.rkt"
  (get-location-name-from-location
   location-id)])

(lazy-require
 ["place.rkt"
  (place-id
   place-shortname
   place-visited?)])

(lazy-require
 ["../action.rkt"
  (action-details)])

(lazy-require
 ["state/state.rkt"
  (current-pending-action
   get-pending-traverse-direction)])

(serializable-struct
 route
 ([id #:mutable] ; mutable for s11n, but should not be mutated
  [a #:mutable]
  [b #:mutable]
  [details #:mutable]
  [actors #:mutable]
  [features #:mutable])
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
    ['a (set-route-details! route
                            (append-element (route-details route) 'a-visited))]
    ['b (set-route-details! route
                            (append-element (route-details route) 'b-visited))]))

(define (route-traversed? route)
  (memq 'traversed (route-details route)))

(define (route-a-visited? route)
  (memq 'a-visited (route-details route)))

(define (route-b-visited? route)
  (memq 'b-visited (route-details route)))

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
  (set-route-details! route (append-element (route-details route) 'traversed)))

(define (route-fully-known? route)
  (and (route-a-visited? route)
       (route-b-visited? route)
       (route-traversed? route)))

(define (route-has-detail? route detail)
  (memq detail (route-details route)))

(define (route-add-detail route detail)
  (when (not (route-has-detail? route detail))
    (set-route-details! route
                        (append-element (route-details route)
                                        detail))))

(define (route-remove-detail route detail)
  (when (route-has-detail? route detail)
    (set-route-details! route
                        (remove detail
                                (route-details route)))))

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

; this should be broken up; action handling elsewhere
(define (display-route-info-card route)
  (define id (route-id route))
  (define title "Location (en route)")

  (define pending-action (current-pending-action))
  (define details (action-details pending-action))
         
  (define traverse-direction
    (if (memq 'a-to-b details)
        'a-to-b
        'b-to-a))

  (define endpoint
    (case traverse-direction
      ['a-to-b (route-b route)]
      ['b-to-a (route-a route)]))

  (define startpoint
    (case traverse-direction
      ['a-to-b (route-a route)]
      ['b-to-a (route-b route)]))

  
  (define body
    (cond ((route-fully-known? route)
           (prune
            (list
             (list
              (string-append " "
                             (place-shortname startpoint)
                             " – "
                             (place-shortname endpoint)
                             " ")
              (string-append " "
                             "[route]"
                             " "))
             (when (not (null? (route-features route)))
               (list (string-append " "
                                    "features"
                                    " ")
                     (string-append " "
                                    (~v (route-features route))
                                    " "))))))
          (else
           (prune
            (list
             (list
              (string-append " "
                             (get-location-name-from-location startpoint)
                             " – "
                             "???"
                             " ")
              (string-append " "
                             "[route]"
                             " "))
             (when (not (null? (route-features route)))
               (list (string-append " "
                                    "features"
                                    " ")
                     (string-append " "
                                    (~v (route-features route))
                                    " "))))))))
  (info-card body title))
