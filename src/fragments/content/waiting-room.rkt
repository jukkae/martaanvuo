#lang at-exp racket

(provide (all-defined-out))

(require "../../core/api.rkt")


(fragment
 'waiting-room-begin
 (thunk
  (p @~a{
 The waiting room is a short corridor, its walls painted in pale clinical green, coldly lit by a flickering fluorescent. There is a single wood-framed chair along one wall. A bit further down the corridor, there's a single, heavy white door with no markings or signs.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-1
        )))

(fragment
 'waiting-room-1
 (thunk
  (p @~a{
 There's the soft whirr and occasional clanks of air systems. Might as well sit down.

 The chair creaks as its frame slightly flexes. It is not an uncomfortable chair. Its thinly padded seat is upholstered in sturdy, bluish gray fabric.

 When will they call her, she wonders, if it'll be long... What to say. What will it be like. Glimmering, glorious worlds of pure light.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-2
        )))

(fragment
 'waiting-room-2
 (thunk
  (p @~a{
 Every time Otava shifts her position, the chair creaks a bit. It's not an uncomfortable chair, but it is not comfortable, either. The back rest digs annoyingly between the vertebrae if you lean on it too long, and the gently curving arm rests are not really that functional as arm rests.

 It's been a while. They are aware that she is there, right? They're just busy and they'll call her when they're ready. The book said this is the penultimate step, and just enter the waiting room and wait, which is what she's doing right now, right? So they'll soon call her, then.

 She's freaking out a bit, but this all makes sense, so she sits back down on the annoyingly creaky, back-hurting excuse of a chair.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-3
        )))

(fragment
 'waiting-room-3
 (thunk
  (p @~a{
 A while passes. She'll knock on the door.

 She stands up. The chair creaks. She takes a couple steps down the corridor, then walks it down in its entirety. It is not very long, a dozen or so paces. There is just the one door along the corridor, and the chair.

 She thinks if she really should knock. The book said nothing about knocking. She knows she shouldn't, but, like, *should* she?

 Maybe she should. How do they know she's here? Well, they just know, they have their means. It is so. How does she know this?

 Or, you know maybe it's just a mistake. She should let them know, nevermind, let's go back.

 But, what if it's not a mistake? What if they'll be angered for not following the, admittedly extremely simple, instructions?

 She shouldn't knock, not without thinking this through.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-4
        )))

(fragment
 'waiting-room-4
 (thunk
  (p @~a{
 How long has it been now? An hour? Two? Something like that.

 There's a misunderstanding, clearly. She should knock.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-5
        )))

(fragment
 'waiting-room-5
 (thunk
  (p @~a{
 She walks to the door again, raises her hand to rap on the door. What is going to happen?

 She does not know.

 The best case? Wherever that door leads, obviously, but she'll take going back, too.

 The worst case? Who the fuck knows, the instructions said nothing about knocking, but by not knocking she will certainly do no harm.

 It is clearly, objectively better not to knock. She can knock whenever, later, no harm done, but she can never un-knock. It said "wait", not "wait and knock". They'll call her eventually.

 Fine.

 She lets her hand fall back down and goes back to the chair.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-6
        )))

(fragment
 'waiting-room-6
 (thunk
  (p @~a{
 She waits.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-7
        )))

(fragment
 'waiting-room-7
 (thunk
  (p @~a{
 Would she even be able to knock on the door if she wanted? It said that you can only wait, yet she has done many things: She has sat, she has breathed, perceived. She has thought – is thinking not an action, too? – and she has stood up and walked. She's basically almost knocked on the door already, and the only reason she hasn't done so already is because she thinks it's better not to do it, not because she couldn't!

 There is no reason why she would not be able to knock on the goddamn door.

 She just chooses not to, exercising her own damn free will, because it is the most logical course of action.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-8
        )))

(fragment
 'waiting-room-8
 (thunk
  (p @~a{
 She waits, long, at least an hour. This is getting excessive. She doesn't even know how long she should wait! The whole process has been very confusing!

 She stands up and goes to the door. This time, she'll do it.

 She walks up to the door, sizing it up. Well then, door, it's just you and me, and this waiting room is too small for the two of us.

 Should she think of something to say? "Hello" is, of course, a classic, but it exudes a certain lack of gravitas. It is not... refined. Asking questions makes her look not ready.

 Ah, she'll announce herself if somebody asks.

 She knocks on the door, and the door resonates with a satisfying, chonky sound.

 That is good, because of two things:

 One: That she was able to do it. Not that she ever really doubted if she could, not really, ha ha, she knew that she could but just chose not to.

 Two: The door is not soundproof.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-9
        )))

(fragment
 'waiting-room-9
 (thunk
  (p @~a{
 Ha ha ha, if they never open, if she gets bored of the waiting, maybe she'll start simulating the universe in her head. It doesn't need to be a particularly fancy one, at first. Just lay down some basic foundations and see where they lead.

 Ha ha, good idea, very funny.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-10
        )))

(fragment
 'waiting-room-10
 (thunk
  (p @~a{
 The walls are actually not quite as solidly coloured as she originally thought: There's some yellowing high up, next to where the wall meets the ceiling. There's a couple of spots where the white base coat is showing through. A few scratches, dark marks, scuffs and dents. Not in bad shape by any means, but not pristine.

 She's trying to figure out if there's a pattern to the flickering of the light. She thinks there is, only to be proven to be wrong when the light suddenly flickers when it's not supposed to, or fails to flicker when it should.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-11
        )))

(fragment
 'waiting-room-11
 (thunk
  (p @~a{
 Because if there is a pattern, then everything else follows: She can figure things out, predict, test, and eventually work it out.

 But if there is no pattern, then she's fumbling in the dark: Laws of cause and effect won't hold, and whatever she does or does not do anyway, it doesn't matter.

 Ah, maybe that was what was meant by the whole "won't be able to do nothing but wait" thing: Not that it's not possible, but that it doesn't make sense, that it doesn't make a difference.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-12
        )))

(fragment
 'waiting-room-12
 (thunk
  (p @~a{
 She waits.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-patterns-1
        )))

(fragment
 'waiting-room-patterns-1
 (thunk
  (p @~a{
 Ah how she wishes that she’s in a world that makes sense! Even if it’s a cruel one, if it is deterministic then there are things that she can do!

 She still has her free will, does she not? She has proven it, time and again!

 She thinks, therefore she is, and because the world appears, then the world must also be! Is she mistaken? She is not!
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-patterns-2
        )))

(fragment
 'waiting-room-patterns-2
 (thunk
  (p @~a{
 Except...

 Can she really know this? She’s definitely real – she’s been real, she’s got history.

 But where is that history now? All the people, the good times and the bad, the conversations, where are they? Can they be touched? No? Can they be seen?
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-patterns-3
        )))

(fragment
 'waiting-room-patterns-3
 (thunk
  (p @~a{
 What if the memories have been planted in your mind just a minute ago? How could you tell the difference? Otava, how would you know if you were programmed not to know?
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-patterns-4
        )))

(fragment
 'waiting-room-patterns-4
 (thunk
  (p @~a{
 Well, anyway, she’s here now.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-13
        )))



(fragment
 'waiting-room-13
 (thunk
  (p @~a{
 She waits.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-14
        )))

(fragment
 'waiting-room-14
 (thunk
  (p @~a{
 She waits. The light flickers.

 On cue.

 The light doesn't flicker.

 On cue.

 Long wait, as expected.

 Still, the light doesn't flicker, and there's a clank of AC and a flicker. Fuck!

 Oh, but the AC! She had forgotten about the AC! What if it's the AC that causes the flickers?
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-15
        )))

(fragment
 'waiting-room-15
 (thunk
  (p @~a{
 She waits. The light flickers and doesn't flicker, the AC hums and whirrs and clanks and the chair creaks a bit whenever she changes position.

 It doesn't seem to be entirely random – it locks onto a pattern for a while, only to find new devious ways of doing something unexpected.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-16
        )))

(fragment
 'waiting-room-16
 (thunk
  (p @~a{
 Fine. Be unpredictable if you like. I'll figure something out.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-17
        )))

(fragment
 'waiting-room-17
 (thunk
  (p @~a{
 She waits and the AC clanks and hums and whirrs unpredictably. The light mostly doesn't flicker. There seems to be nothing to do.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-18
        )))

(fragment
 'waiting-room-18
 (thunk
  (p @~a{
 She goes to the door and knocks again.

 After a while, she knocks louder.

 She turns the handle, shifting her weight to push the door open.

 The door doesn't budge. It is locked.

 It can't be locked. She tries again.

 Locked.

 She's just not doing it right. She turns the handle again.

 Locked.

 Oh come on now, nothing in this goddamn hellscape makes sense, nothing follows rules, so you should not follow rules you silly fucking piece of artificial wood –

 Still locked.

 – She shackes the handle violently. It feels a bit loose. The door rattles. Otava gives it a kick, but it doesn't help.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-19
        )))

(fragment
 'waiting-room-19
 (thunk
  (p @~a{
 She waits.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-20
        )))

(fragment
 'waiting-room-20
 (thunk
  (p @~a{
 Simulating the universe is starting to sound pretty appealing.

 Ah, but it is one thing to think it, and another thing entirely to actually do it! To do it would be to actually do it, and she hasn't done it yet!
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-lift-handle-1
        )))

(fragment
 'waiting-room-lift-handle-1
 (thunk
  (p @~a{
 Wait. She never tried to *lift* the handle, only press it down! Maybe it’s one of those kind of handles, or somebody installed it wrong or something!

 The thought fills her with elation: Saved! At last! It is not permanent, it can be undone, and she’ll soon be free again! Fuck ascending to the next plane of existence or whatever, she’ll be happy fighting blindscrapers the rest of her life if she just gets this one thing.

 She smiles. So simple! How did she not think of it in the first place! Things like this happen to the best of us. She’ll just walk over, lift the handle up, open the door and go out.

 Perfect.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-lift-handle-2
        )))

(fragment
 'waiting-room-lift-handle-2
 (thunk
  (p @~a{
 Except...
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-lift-handle-3
        )))

(fragment
 'waiting-room-lift-handle-3
 (thunk
  (p @~a{
 Deep down inside she knows this is not how it is going to be.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-lift-handle-4
        )))

(fragment
 'waiting-room-lift-handle-4
 (thunk
  (p @~a{
 She knows that when she tries to lift the handle, it won’t rise more than a smidgen. There’s a bit of play in it, sure, so it’ll certainly feel like it would open, giving that one last bit of false hope, but it won’t open.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-lift-handle-5
        )))

(fragment
 'waiting-room-lift-handle-5
 (thunk
  (p @~a{
 But not trying it makes it not real. If she doesn’t know, if she’s not sure, she still has hope.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-lift-handle-6
        )))

(fragment
 'waiting-room-lift-handle-6
 (thunk
  (p @~a{
 She’ll think about this a bit.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-21
        )))

(fragment
 'waiting-room-21
 (thunk
  (p @~a{
 She waits.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-dead-end-1
        )))

(fragment
 'waiting-room-dead-end-1
 (thunk
  (p @~a{
 Fucking hell, man. Only one damn metaphysical dead end in the whole wide world and of course she's got to find it.

 Fuck.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'dead-end-2
        )))

(fragment
 'waiting-room-dead-end-2
 (thunk
  (p @~a{
 So much left to see, to do... she had a life! Sure, there were some fucking abysmal times, the debt and hustle, but on the whole, she was enjoying it. Who knows, maybe things might've changed for the good.

 Maybe they are changing for the good, if not for her, then... you know, there's still plenty of people out there living their lives. Some of them have OK lives, hopefully.

 Maybe the world has stopped turning into even more shit.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'dead-end-3
        )))

(fragment
 'waiting-room-dead-end-3
 (thunk
  (p @~a{
 Maybe not.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-22
        )))

(fragment
 'waiting-room-22
 (thunk
  (p @~a{
 There's nothing to do for Otava but to wait.

 So she waits.

 And then there's the one other thing.

 Hold on, what other thing?

 The, you know, the other thing. No no no no, not killing yourself, you wouldn't be able to do it here anyway, this is the waiting room, not the waiting a bit and then killing yourself room. And even if you did, you'd still be stuck in Martaanvuo.

 No, Otava, the *other* thing, the except-for, the other-than.

 You know. The wordless the-beyond, the not-of-this-reality.

 Think on it.

 The door is real. It is locked and it can't be broken down.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-end
        )))

(fragment
 'waiting-room-end
 (thunk
  (p @~a{
 She waits.
 }))
 #:decisions
 (list (make-decision
        #:title "Wait."
        #:next-fragment 'waiting-room-end
        )))