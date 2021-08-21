#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "io.rkt")
(require "utils.rkt")

(lazy-require
 ["location.rkt"
  (get-location-name-from-location
   location-id)])

(lazy-require
 ["place.rkt"
  (place-id
   place-visited?)])

(serializable-struct
 route
 ([id #:mutable] ; mutable for s11n, but should not be mutated
  [a #:mutable]
  [b #:mutable]
  [details #:mutable]
  [actors #:mutable])
 #:transparent
 #:constructor-name route*)

(define
  (make-route id
              a
              b
              #:details [details '()]
              #:actors [actors '()])

  (route* id
          a
          b
          details
          actors))



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
         ['magpie-hill "Magpie and the rocky slope."]
         ['martaanvuo-swamp "Ants and the swamp."]
         [else (string-append "["
                              (symbol->string (location-id start-location))
                              " to "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]
      ['magpie-hill
       (case (location-id (route-other-end-from route start-location))
         ['perimeter "Rocky stairs."]
         ['martaanvuo-swamp "Down, toward the swamp."]
         ['power-plant-ruins "Decrepit building."]
         [else (string-append "["
                              (symbol->string (location-id start-location))
                              " to "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]
      ['martaanvuo-swamp
       (case (location-id (route-other-end-from route start-location))
         ['magpie-hill "The hill."]
         ['perimeter "The path."]
         [else (string-append "["
                              (symbol->string (location-id start-location))
                              " to "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]
      ['martaanvuo-swamp
       (case (location-id (route-other-end-from route start-location))
         ['magpie-hill "Magpie and the rocky slope."]
         ['perimeter "Rocky stairs."]
         [else (string-append "["
                              (symbol->string (location-id start-location))
                              " to "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]
      ['power-plant-ruins
       (case (location-id (route-other-end-from route start-location))
         ['cache "The previously locked door."]
         ['sewers-1 "The sewers."]
         [else (string-append "["
                              (symbol->string (location-id start-location))
                              " to "
                              (symbol->string (location-id (route-other-end-from route start-location)))
                              "]")])]
      [else
       
       (string-append "["
                      (symbol->string (location-id start-location))
                      " to "
                      (symbol->string (location-id (route-other-end-from route start-location)))
                      "]")]))
  

  
  (cond ((route-fully-known? route)
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
         (get-route-short-description))))

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


(define (display-route-info-card route)
  (define id (route-id route))
  (define title "Location (en route)")
  (define body
    (list
     (list " aa " " bb "))
    #;(prune (list
            (when (not (null? id))
              (list (string-append " "
                                   "id"
                                   " ")
                    (string-append " "
                                   (cond ((number? id) (number->string id))
                                         ((symbol? id) (symbol->string id)))
                                   " ")))
            )))
  (info-card body title))


