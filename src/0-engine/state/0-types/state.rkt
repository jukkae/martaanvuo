#lang typed/racket

(provide (all-defined-out))

(require
  "../../2-core/maybe.rkt"
  "../../3-types/action.rkt"
  "../../actors/0-types/pc-actor.rkt"
  "../../combat/combat-event.rkt"
  "../../3-types/location-ids.rkt"
  "../../resolvers/round-resolver/0-types/timeline.rkt"
  "../../tasks/task.rkt"
  "../../world/0-types/world.rkt")

(struct State
  ([world : world]
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
   [round : Natural]
   [run : Natural]
   [elapsed-time : Natural] ; should be in-world time
   [in-combat? : Boolean]
   [tasks : (Listof task)]
   [pc : pc-actor]
   [life : Natural]
   [current-fragment-id : (Maybe Symbol)] ; Symbol -> FragmentId
   [completed-fragments : (Listof Symbol)] ; Symbol -> FragmentId
   [combat-timeline : (Maybe (Listof combat-event))]
   [show-round-summary? : Boolean]
   )
  #:prefab
  #:mutable)
