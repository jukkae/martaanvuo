#lang racket

(define *loot* '('amulet 'knife))

(define item<%> (interface () get-inline-description get-short-description get-uses get-condition))

(define amulet%
  (class* object% (item<%>)
    (super-new)

    (define/public (get-inline-description) "a crow's feather amulet")
    (define/public (get-short-description) "crow amulet")
    (define/public (get-uses) null)
    (define/public (get-condition) 'good)))

(define knife%
  (class* object% (item<%>)
    (super-new)

    (define/public (get-inline-description) "an obsidian cutting stone")
    (define/public (get-short-description) "obsidian blade")
    (define/public (get-uses) '(strike))
    (define/public (get-condition) 'good)))



; TODO: define in terms of describable<%>
(define (get-list-inline-description list)
  (if (empty? list)
      "nothing"
      (string-append (send (car list) get-inline-description)
                     (cond ((= (length list) 1) "")
                           ((= (length list) 2) (string-append " and " (get-list-inline-description (cdr list))))
                           (else (string-append ", " (get-list-inline-description (cdr list))))))))


(provide (all-defined-out))

