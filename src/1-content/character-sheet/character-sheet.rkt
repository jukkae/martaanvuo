#lang at-exp racket

(provide character-sheet
         display-inventory)

(require racket/lazy-require)

(require "../../0-engine/1-index/state.rkt"

         "../../0-engine/2-core/io.rkt"
         "../../0-engine/2-core/core.rkt"

         "../../0-engine/3-types/actor.rkt"
         "../../0-engine/3-types/condition.rkt"
         "../../0-engine/3-types/item.rkt"
         "../../0-engine/3-types/manipulator.rkt"
         "../../0-engine/3-types/modification.rkt"
         "../../0-engine/3-types/pc-actor.rkt"
         "../../0-engine/3-types/sense-organ.rkt"

         "../../0-engine/4-systems/items/item.rkt"
         )

; Eventually, maybe lift this to the same "level" as numbered options. Then:
; Character sheet could be a treelike menu, where also letters are used.
; '-' is used to go back one level.
; Then, concrete actions with diegetic effects should return an action ultimately for (get-next-pc-action).
; Otherwise, return #t or whatever's appropriate for round-resolver
(define (character-sheet)
  (display-character-sheet)
  (when (not (empty? (pc-actor-modifications (pc))))
    (display-modifications))
  (display-sense-organs)
  (display-manipulators)
  (display-inventory)
  ; TODO: until interaction-result is == go-back-to-game loop: select-interaction-target
  (define interaction-target (select-interaction-target))
  (when (not (null? interaction-target))
    (interaction-target))
  #t)

(define (display-character-sheet)
  (define actor (pc))

  (define sheet
    (tbody (tr (format "~a ~a~a"
                       (actor-name actor)
                       (first (current-epithets))
                       (if (not (empty? (cdr (current-epithets)))) "," ""))
               "")
           (tr (string-append* (add-between (cdr (current-epithets)) ", ")) "")
           (tr "" "")
           #;(tr "HP" (format "~a/~a" (actor-hp actor) (actor-max-hp actor)))
           (tr "HP" "[unknown]") ; TODO: acquire nociception
           (tr "Size" "the size of Otava")
           ;  (tr "Size" (format "~a" (actor-size actor)))
           ))

  (when (not (= 0 (pc-actor-xp actor)))
    (set! sheet (append-element sheet (tr "XP" (number->string (pc-actor-xp actor))))))

  (define traits (actor-traits actor))
  (define traits-list
    (for/list ([(k v) (in-hash traits)])
      (tr k (number->string v))))

  ; append emptyline above
  (when (not (null? traits-list))
    (set! traits-list (cons (tr "" "") traits-list)))

  (define conditions (actor-conditions actor))
  (define conditions-list
    (for/list ([condition conditions])
      (tr (format "[~a]" (condition-type condition)) "")))

  ; append emptyline above
  (when (not (null? conditions-list))
    (set! conditions-list (cons (tr "" "") conditions-list)))

  (define hunger-list
    (list (tr "" "")
          (tr "hunger" (number->string (pc-actor-hunger actor)))
          (tr "fatigue" (number->string (pc-actor-fatigue actor)))))

  (set! sheet (append sheet #;attributes-list traits-list conditions-list hunger-list))

  (info-card sheet "Character sheet"))

(define (display-inventory)
  (define actor (pc))

  (define header (tbody (tr "Item" "Quantity" "Notes")))

  (define items (actor-inventory actor))
  (define items-list
    (for/list ([item items])
      (cond
        [(symbol? item) (tr (format "~a" item) (~v (item-quantity item)) "")]
        [(ranged-weapon? item)
         (tr (item-name item)
             (~v (item-quantity item))
             (format "ammo left: ~a" (ranged-weapon-ammo-left item)))]
        [(equal? (item-id item) 'bolt-cutters) (tr (item-name item) (~v (item-quantity item)) "")]
        [(item? item)
         (tr (item-name item)
             (~v (item-quantity item))
             (if (not (null? (item-details item))) (~v (item-details item)) ""))]
        [else (tr (symbol->string item) (~v (item-quantity item)) "")])))

  (define sheet (append header items-list))

  (info-card sheet "Inventory"))

(define (select-item)
  (define items (actor-inventory (pc)))

  (prln (format "Select item [1-~a] to inspect or use, anything else to cancel." (length items)))
  (br)

  (for ([item items] [i (in-naturals 1)])
    (if (= (item-quantity item) 1)
        (prln (format "[~a] ~a" i (item-name item)))
        (prln (format "[~a] ~a (~a)" i (item-name item) (item-quantity item))) ; TODO: pluralized
        ))
  (br)
  (define input (string->number (wait-for-input)))
  (define selected-item '())
  (cond
    [(and (number? input) (> input 0) (<= input (length items)))
     (define index (- input 1))
     (set! selected-item (list-ref items index))]
    [else
     '()
     (notice "Nevermind.")])
  (when (not (null? selected-item))
    (item-submenu selected-item)))

(define (item-submenu selected-item)
  (define targets (list (cons "Inspect" (λ () (inspect-item selected-item)))))
  (for ([verb (item-interaction-verbs selected-item)])
    (set! targets (append-element targets (cons (format "~a" verb) (λ () (interact-with-item verb selected-item))))))

  (for ([target targets] [i (in-naturals 1)])
    (prln (format "[~a] ~a" i (car target))))
  (br)

  (define input (string->number (wait-for-input)))
  (cond
    [(and (number? input) (> input 0) (<= input (length targets)))
     (define index (- input 1))
     ((cdr (list-ref targets index)))]
    [else
     '()
     #;(p "Nevermind.")])


  #;(dev-note (format "Item: ~a" selected-item))
  (wait-for-confirm))

(define (display-modifications)
  (define actor (pc))

  (define header (tbody (tr "Modification" "Details")))

  (define modifications (pc-actor-modifications actor))
  (define modifications-list
    (for/list ([modification modifications])
      (tr (Modification-name modification) (~v (Modification-details modification)))))

  (define sheet (append header modifications-list))

  (info-card sheet "Modifications"))

(define (display-sense-organs)
  (define actor (pc))

  (define sense-organs (pc-actor-sense-organs actor))
  (define sense-organs-list
    (for/list ([sense-organ sense-organs])
      (tr (format "~a" (SenseOrgan-name sense-organ))
          (format "lv ~a" (SenseOrgan-level sense-organ)))))

  (when (empty? sense-organs-list)
    (set! sense-organs-list (tr "No sense organs")))

  (info-card sense-organs-list "Sense organs"))

(define (display-manipulators)
  (define actor (pc))

  (define manipulators (pc-actor-manipulators actor))
  (define manipulators-list
    (for/list ([manipulator manipulators])
      (tr (Manipulator-name manipulator))))

  (when (empty? manipulators-list)
    (set! manipulators-list (tr "No manipulators")))

  (info-card manipulators-list "Manipulators"))

(define (use-skill)
  (define skills '(#;switch-perspective))
  (cond
    [(empty? skills) (notice "No skills.")]
    [else
     (prln (format "Select skill [1-~a], anything else to cancel." (length skills)))
     (br)

     (for ([skill skills] [i (in-naturals 1)])
       (prln (format "[~a] ~a" i skill)))
     (br)
     (define input (string->number (wait-for-input)))
     (define selected-skill '())
     (cond
       [(and (number? input) (> input 0) (<= input (length skills)))
        (define index (- input 1))
        (set! selected-skill (list-ref skills index))]
       [else
        '()
        #;(p "Nevermind.")])
     (when (not (null? selected-skill))
       (dev-note (format "Skill: ~a" selected-skill))
       (case selected-skill
         ['switch-perspective (toggle-flag 'perspective-switched)]))]))

; TODO: These must be filtered w.r.t combat
(define (select-interaction-target)
  (define targets (list (cons "Select item to inspect or interact with" select-item) (cons "Use skill" use-skill)))

  (prln (format "Select [1-~a], or anything else to go back to game." (length targets)))
  (br)

  (for ([target targets] [i (in-naturals 1)])
    (prln (format "[~a] ~a" i (car target))))
  (br)
  (define input (string->number (wait-for-input)))
  (cond
    [(and (number? input) (> input 0) (<= input (length targets)))
     (define index (- input 1))
     (cdr (list-ref targets index))]
    [else
     '()
     #;(p "Nevermind.")]))
