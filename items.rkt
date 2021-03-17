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

(define sapling-finger%
  (class* object% (item<%>)
    (super-new)

    (define/public (get-inline-description) "a mummified human finger")
    (define/public (get-short-description) "a human finger")
    (define/public (get-uses) null)
    (define/public (get-condition) 'good)))

(define knife%
  (class* object% (item<%>)
    (super-new)

    (define/public (get-inline-description) "an obsidian cutting stone")
    (define/public (get-short-description) "obsidian blade")
    (define/public (get-uses) '(stab))
    (define/public (get-condition) 'good)))

(define twine%
  (class* object% (item<%>)
    (super-new)

    (define/public (get-inline-description) "a length of twine, spun from an unknown material")
    (define/public (get-short-description) "a ball of twine")
    (define/public (get-uses) null)
    (define/public (get-condition) 'good)))


(provide (all-defined-out))

