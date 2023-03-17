#lang typed/racket

(provide (struct-out State))

(require "../../2-core/maybe.rkt"

         "../../3-types/action.rkt"
         "../../3-types/combat-event.rkt"
         "../../3-types/location-ids.rkt"
         "../../3-types/pc-actor.rkt"
         "../../3-types/route.rkt"
         "../../3-types/task.rkt"
         "../../3-types/timeline.rkt")

(require/typed
 "../../3-types/world.rkt"
 [#:struct world
  ([places : (Listof Place)] [routes : (Listof route)] [day : Natural] [elapsed-time : Natural])])

(define-type Rng-state-vector (Vector Integer Integer Integer Integer Integer Integer))

(struct State
        ([rng-seed : Integer]
         [rng-state : Rng-state-vector]
         [world : (Maybe world)]
         [last-numeric-actor-id : Natural]
         [log : (Listof String)]
         [last-paragraph : String]
         [part : Natural]
         [chapter : Natural]
         [prompt : String]
         [pending-action : (Maybe action)]
         [times-begin-traverse-narrated : (HashTable (List LocationId LocationId) Natural)]
         [times-finish-traverse-narrated : (HashTable (List LocationId LocationId) Natural)]
         [times-cancel-traverse-narrated : (HashTable (List LocationId LocationId) Natural)]
         [times-species-encountered : (HashTable Symbol Natural)]
         [flags : (Listof Symbol)]
         [counters : (HashTable Symbol Natural)]
         [round : Natural]
         [run : Natural]
         [recursion-depth : Natural]
         [elapsed-time : Natural] ; should be in-world time
         [in-combat? : Boolean]
         [tasks : (Listof task)]
         [epithets : (Listof String)]
         [once-per-day-actions-done : (Listof Symbol)]
         [life : Natural]
         [current-fragment-id : (Maybe Symbol)] ; Symbol -> FragmentId
         [completed-fragments : (Listof Symbol)] ; Symbol -> FragmentId
         [combat-timeline : (Maybe (Listof combat-event))]
         [show-round-summary? : Boolean])
  #:prefab
  #:mutable)
