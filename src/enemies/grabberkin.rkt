#lang racket

(provide (all-defined-out))

(require racket/lazy-require)
(require racket/serialize)

(require "../state/combat.rkt"
         "../state/state.rkt")

(require "../action.rkt"
         "../actor.rkt"
         "../io.rkt"
         "../stance.rkt"
         "../status.rkt"
         "../utils.rkt"
         "../world.rkt")


; Grabberkin are greedy -> they have jewelry -> they drop loot

;;; THE BIG THINGS:
;;; What is Grabberkin's purpose, gameplay-wise? What is its niche?
;;; What is its vulnerability?
;;; How much leeway there is? Should it be considered a mini-boss?
;;; Idea: Easy to win once you know how, but requires something specific that can be gotten easily if you know how
;;; -> act as a soft gate
;;; -> somewhat miniboss-like *at first*
; They are greedy. They are clingy. (What do they most wish?)
; Add 'steal' action, but make it pretty rare / once-per-encounter type thing maybe? (steal gold, not items)
; Idea: Vulnerabilities:
; - Firearms: Simply too much firepower. (Approach weaknesses: Makes noise, bullets are hard to come by, fuck up and blow your own leg up -> skill check if not proficient / consume LP)
; - Chainsaw: Too awesome. Weaknesses: Got to get up-and-close, makes noise, needs gas
; - Slashing damage, IF you know how to do it - because it cuts through tendons -> can't grab no more
; - Good perception (-> avoid)

; -> when selecting loadout, possibilities are bolt cutters or a chainsaw,
; gun has to be sourced by doing a favor to The Merchant, robbing a Cache, or finding your own corpse that had a gun in a prior run.
; guns: hunting rifles and shotguns in the wild, at the Facility also handguns and assault rifles.

; This doesn't belong here really but what the hell:
; - After combat, patching up is possible, but it is risk-free only if you know medicine, which requires finding literature (= doing a Cache-run, in practice) and then leveling up


(define (make-grabberkin-action actor action-flag)
  (case action-flag
    ['pull-under
     (make-action
      #:symbol 'pull-under
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details '())]

    ['anklebreaker
     (make-action
      #:symbol 'anklebreaker
      #:actor actor
      #:duration 1
      #:target (pc)
      #:tags '(initiative-based-resolution)
      #:details '())]
    
    ['skip
     (make-action
      #:symbol 'skip
      #:actor actor
      #:duration 0
      #:target '()
      #:tags '(initiative-based-resolution)
      #:details '(slow silent))]

    ['grab
     (define strength (+ (d 1 4) 1))
     (make-action
      #:symbol 'modify-status
      #:actor actor
      #:duration 0
      #:target (pc)
      #:tags '(initiative-based-resolution fast)
      #:details (list (status 'bound strength)))] ; this is shit, refactor

    ['release-grip
     (make-action
      #:symbol 'release-grip
      #:actor actor
      #:duration 0
      #:target '()
      #:tags '(initiative-based-resolution)
      #:details '(fast))]

    [else
     (error (string-append
             "make-grabberkin-action: unknown action: "
             (symbol->string action-flag)))]))

(define (grabberkin-hp-above-threshold? actor)
  (define hp (actor-hp actor))
  (define max-hp (actor-max-hp actor))
  (define threshold (quotient max-hp 2))
  
  (> hp threshold))

(define (get-grabberkin-action-phase-1 actor)
  (cond
    ((and (actor-in-range? actor 'engaged)
          (actor-has-status-of-type? (pc) 'bound)
          (> (actor-lifetime-of-status-of-type? (pc) 'bound)
             4))
     (define options
       '(anklebreaker anklebreaker grab skip skip skip))
               
     (define roll (d 1 6))
     (define index (- roll 1))
     (define action (list-ref options index))

     (make-grabberkin-action actor action))
    (else
     (make-grabberkin-action actor 'grab))))

(define (get-grabberkin-action-phase-2 actor)
  (cond
    ((and (actor-in-range? actor 'engaged)
          (actor-has-status-of-type? (pc) 'bound)
          (> (actor-lifetime-of-status-of-type? (pc) 'bound)
             4))
     (define options
       '(pull-under pull-under grab grab skip skip))
               
     (define roll (d 1 6))
     (define index (- roll 1))
     (define action (list-ref options index))

     (make-grabberkin-action actor action))
    (else
     (make-grabberkin-action actor 'grab))))

(define (get-grabberkin-action actor)
  (cond ((in-combat?)
         (cond
           ((grabberkin-hp-above-threshold? actor)
            (define target (pc))
            (define phase
              (cond
                ((and (not (actor-has-condition-of-type? target 'ankle-broken))
                      (not (actor-has-condition-of-type? target 'both-ankles-broken)))
                 1)
                ((and (actor-has-condition-of-type? target 'ankle-broken)
                      (not (actor-has-condition-of-type? target 'both-ankles-broken)))
                 1)
                ((and (not (actor-has-condition-of-type? target 'ankle-broken))
                      (actor-has-condition-of-type? target 'both-ankles-broken))
                 2)))
            
            (case phase
              [(1) (get-grabberkin-action-phase-1 actor)]
              [(2) (get-grabberkin-action-phase-2 actor)]))
           
           (else
            (make-grabberkin-action actor 'release-grip))))
        (else
         (begin (displayln "Grabberkin AI, not in combat")))))

; implicitly, this is the pre-own-action reaction
(define (get-grabberkin-reaction actor)
  (cond ((not (grabberkin-hp-above-threshold? actor))
         (make-grabberkin-action actor 'release-grip))
        (else
         '())))

(define (spawn-grabberkin-encounter!)
  
  (p "Something grabs Otava by the ankle and pulls. She staggers, barely manages to stay upright, and immediately goes for her bolt cutters.") ; could cause fall-down on failed roll
  (begin-combat!)

  (define hp 11)
  (define i 0)
  (define enemy (make-actor "Grabberkin" hp))
  (set-actor-dexterity! enemy 4)
  (set-actor-strength! enemy 11)
  (set-trait! enemy "defense" -1)
  (set-trait! enemy "melee-attack-skill" 1)
  (set-trait! enemy "hp-hidden" #f)
  (move-actor-to-location! enemy (current-location))

  (inflict-status! (pc) (status 'bound 10))

  (define sign
    (case i
      [(0) "α"]
      [(1) "β"]
      [(2) "γ"]
      [(3) "δ"]
      [else ""]))
  (define range 'engaged)
  (define description "grabbing Otava's ankle")
  (define enemy-stance
    (stance sign range description))
           
  (set-actor-stance! enemy enemy-stance)
  )