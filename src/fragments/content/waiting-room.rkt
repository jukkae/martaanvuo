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
 A while passes. 

 Then, she decides to stand up, maybe walk a bit.

 She stands up. The chair creaks. She takes a couple steps down the corridor, then walks it down in its entirety. It is not very long, as far as corridors go, maybe five or ten meters, or so. There is just the one door along the corridor, and the chair.

 She thinks if she should knock. She knows she shouldn't, but, like, *should* she?

 Maybe she should. How do they know she's here? Well, they just know, they have their means. It is so. How does she know this?

 Otava begins doubting herself.

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
 How long has it been now? An hour? Two? Something like that. She drums her fingers together, tries a couple of new configurations how to put them next to each other. No sudden insights emerge.

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
 She walks to the door again, raises her hand to rap on the door. What would this lead to?

 She does not know.

 By not knocking she will certainly do no harm. By knocking it is not certain whether or not harm or benefit will be caused.

 It is clearly, objectively better not to knock, then.

 Fine.

 She lets her hand fall back down, and walks back to the chair.
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
 Would she even be able to knock on the door if she wanted? It said that "only waiting is possible", yet she has done many things: She has sat, she has breathed, perceived. She has thought – is thinking not an action, too? – and she has stood up and walked. She's basically almost knocked on the door already, and the only reason she hasn't done so already is because she thinks it's better not to do it, not because she couldn't!

 There is no reason why she would not be able to knock on the goddamn door.

 She just chooses not to.
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
 She waits. The answers are not coming to her. So then, she must go to the answer.

 She stands up and goes to the door. This time, she'll do it.

 She walks up to the door, sizing it up. Well then, door, it's just you and me, and this waiting room is too small for the two of us.

 Should she think of something to say? "Hello" is, of course, a classic, but it exudes a certain lack of gravitas. It is not... refined. Asking questions makes her look not ready.

 Ah, she'll do the confident thing, just announce herself as she enters.

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
        #:next-fragment 'waiting-room-22
        )))

(fragment
 'waiting-room-22
 (thunk
  (p @~a{
 There's nothing to do for Otava but to wait.

 And then there's the one other thing.

 Wait, who said that? What other thing?

 The, you know, the other thing. No no, not killing yourself, you wouldn't be able to do it here anyway, this is the waiting room, not the waiting a bit and then killing yourself room.

 No, Otava, the *other* thing, the *except for*.

 You know. The wordless the-beyond.

 Think on it.

 Or, you can always wait.
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