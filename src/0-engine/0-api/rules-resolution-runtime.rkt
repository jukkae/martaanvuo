#lang racket

(require reprovide/reprovide)
(require reprovide/require-transformer/glob-in)

(reprovide
  "../1-index/content.rkt"

  "../2-core/io.rkt"


  "../3-types/decision.rkt"
  "../3-types/item.rkt"
  "../3-types/location.rkt"
  "../3-types/place.rkt"
  "../3-types/pc-actor.rkt"
  "../3-types/world.rkt"

  "../4-systems/actors/statuses.rkt"
  "../4-systems/blurbs/blurbs.rkt"
  "../4-systems/checks/checks.rkt"
  "../4-systems/events.rkt"
  "../4-systems/fragments.rkt"
  "../4-systems/items/item.rkt"
  "../4-systems/locations/locations.rkt"
  "../4-systems/locations/routes.rkt"
  "../4-systems/pc/pc.rkt"
  "../4-systems/world/time.rkt"
  "../4-systems/world/world.rkt"

  "../6-combat/combat.rkt"
  "../6-combat/combat-action-resolver.rkt"

  "../../1-content/narration/combat-narration.rkt"
  )
