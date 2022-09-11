#lang at-exp racket

(require "../../0-engine/0-api/api.rkt")

(provide display-place-info-card)
(define (display-place-info-card location [title "Location"])
  (define id (location-id location))
  (define body
    (prune (tbody
            (when (not (equal? (Place-shortname location) ""))
              (tr (Place-shortname location)
                  " "))
            (when (not (null? (location-id location)))
              (tr "id"
                  (cond ((number? id) (number->string id))
                        ((symbol? id) (symbol->string id)))))
            (when (and (null? (location-id location))
                       (not (null? (location-type location))))
              (tr "type"
                  (symbol->string (location-type location))))
            (when (not (null? (location-items location)))
              (tr "items"
                  (~v (location-items location))))
            (when (not (null? (location-features location)))
              (tr "features"
                  (~v (location-features location))))
            )))
  (info-card body title))
