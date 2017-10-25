
% ['~/Documents/School/2017-W1/CPSC 312/Project 1/murdermystery.pl'].

%===============================================================================
%===============================================================================
%===============================================================================

% Game code

%===============================================================================
%===============================================================================
%===============================================================================


% picture of game layout: https://imgur.com/a/vGrL4


:- dynamic at/2, i_am_at/1, alive/1, drugs/0, bedroomkey/0, basementkey/0.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(char_met(_)).
:- discontiguous handle_command/1, talk_helper2/1, talk_helper/0, dir_describe/1, describe/1.

% initial position
i_am_at(garage).


% connections between rooms (see pic of game layout above for details)
% n,e,s,w stand for north,south,east,west respectively, can be combined (e.g. nw = northwest, se = southeast)


path(bedroom1, e, hallway).

path(hallway, w, bedroom1).
path(hallway, n, bedroom2).
path(hallway, e, bedroom3) :-
    at(bedroomkey, in_bag).
path(hallway, e, bedroom3) :-
    write('The door is locked... youll need a key to get in.'), nl, fail.
path(hallway, ne, bathroom1).
path(hallway, s, livingroom).

path(bedroom2, s, hallway).

path(bedroom3, w, hallway).

path(bathroom1, sw, hallway).

path(livingroom, n, hallway).
path(livingroom, w, basement) :-
    at(basementkey, in_bag).
path(livingroom, w, basement) :-
    write('The door is locked... youll need a key to get in.'), nl, fail.
path(livingroom, sw, bathroom2).
path(livingroom, s, kitchen).
path(livingroom, e, patio).

path(basement, e, livingroom).

path(bathroom2, ne, livingroom).

path(kitchen, n, livingroom).

path(patio, w, livingroom).
path(patio, e, garage).
path(garage, w, patio).


/* Object declaration */

object(wrench).
object(ribbon).
object(chickendinner).
object(flashlight).
object(businesscard).
object(bedroomkey).
object(basementkey).
object(drugs).
object(ransomnote).



/* Person declaration */

person(maid).
person(assistant).
person(homeowner).
person(marie).
person(allison).
person(guard).
person(gardener).


/* TODO Initial positions of all objects */

at(wrench, garage).
at(ribbon, patio).
at(chickendinner, bedroom3).
at(flashlight, garage).
at(businesscard, bedroom3).
%at(drugs, garage). should be unitialized, the maid gives u these
at(ransomnote, basement).




/* TODO inital positions of all people */

at(maid, kitchen).
at(assistant, livingroom).
at(gardener, garage).
at(marie, bedroom1).
at(guard, hallway).
at(homeowner, bedroom2).
at(allison, basement).


/* Interacting with objects */

take(X) :- 
    at(X, in_bag),
    write('That item is already in your bag.'), 
    nl, !.

take(X) :-
        at(X, in_hand),
        write('You''re already holding it!'),
        nl, !.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        object(X),
        retract(at(X, Place)),
        assert(at(X, in_bag)),
        write('OK.'),
        nl, !.

take(X) :-
        person(X),
        write('You shouldnt pick up a person!'), nl, !.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules describe how to put down an object. */

drop(X) :-
        at(X, in_bag),
        i_am_at(Place),
        retract(at(X, in_bag)),
        assert(at(X, Place)),
        write('OK.'),
        nl, !.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


/* These rules define movement directions as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).

ne :- go(ne).
nw :- go(nw).

se :- go(se).
sw :- go(sw).


go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        look, !.

go(_) :-
        writeln('You can''t go that way.').


/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl,
        notice_people_at(Place),
        nl.



proper_noun(allison).
proper_noun(marie).

/* These rules set up a loop to mention all the objects in your vicinity. */
notice_people_at(Place) :-
    at(X, Place),
    person(X),
    proper_noun(X),
    write(X), write(' is here.'), nl,
    fail.

notice_people_at(Place) :-
    at(X, Place),
    \+ proper_noun(X),
    person(X),
    write('The '), write(X), write(' is here.'), nl,
    fail.

notice_people_at(_).

notice_objects_at(patio) :-
        at(flashlight, X),
        X \= in_bag,
        write('With the lack of light and the storm, its hard to see the finer details of your surroundings.'), nl,
        !.

notice_objects_at(Place) :-
        at(X, Place),
        object(X),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).

/* how to check the items in your bag */

bag :-
    at(X, in_bag),
    write('There is a '), write(X), write(' in your bag.'), nl,
    fail.

bag(_).



/* examine items in your bag */


examine(wrench):-
    at(wrench, in_bag),
    write('A bloodied wrench used to kill the gardener.'), nl,
    write('Light enough to carry, but heavy enough that someone relatively weak could use it to carry out a crime most foul.'), nl,
    fail.

examine(ribbon):-
    at(ribbon, in_bag),
    write('A long blue lacy ribbon, found out on the patio.'), nl, fail.

examine(chickendinner):-
    at(chickendinner, in_bag),
    write('A plate of chicken, rice and vegetables.  Lukewarm.  A bone is left from one piece of chicken,'), nl,
    write('but the rest of the plate is relatively untouched.'), nl, fail.

examine(flashlight):-
    at(flashlight,in_bag),
    write('Useful for seeing in the dark.'), nl, fail.

examine(businesscard):-
    at(businesscard,in_bag),
    write('It says: Jeff Adams - Business Services.'), nl, fail.

examine(bedroomkey):-
    at(bedroomkey,in_bag),
    write('A key to a particular bedroom.'), nl, fail.

examine(basementkey):-
    at(basementkey,in_bag),
    write('A key to a the basement.'), nl, fail.

examine(drugs):-
    at(drugs,in_bag),
    write('The label says childrens multivitamins, but the pills are not dinosaur shaped, as advertized.'), nl, fail.

examine(ransomnote):-
    at(ransomnote,in_bag),
    write('It reads:  I have your daughter.  Send 10MIL to this bank account if you want to see her again.'), nl, fail.

examine(X) :-
    object(X), \+ at(X, in_bag),
    write('That isn''t in your bag').


/* examine people in your area */

examine(maid):-
    i_am_at(Y),
    at(maid,Y),
    write('The household has a live-in maid, they must be quite well off!'), nl, fail.

examine(assistant):-
    i_am_at(Y),
    at(assistant,Y),
    write('The homeowners assistant for business matters.  He looks quite well dressed, but also terribly overworked.'), nl, fail.

examine(homeowner):-
    i_am_at(Y),
    at(homeowner,Y),
    write('The angry, miserable old man who owns the property.'), nl, fail.

examine(marie):-
    i_am_at(Y),
    at(marie,Y),
    write('She must be the daughter of the homeowner.  Her hair is done up with a lacy, blue ribbon.'), nl, fail.

examine(allison):-
    i_am_at(Y),
    at(allison,Y),
    write('Its allison!  The missing younger daughter of the house has been found!'), nl, fail.

examine(guard):-
    i_am_at(Y),
    at(guard,Y),
    write('A bulky, simple looking guy.  Hes watching you carefully... dont do anything stupid....'), nl, fail.

examine(gardener):-
    i_am_at(Y),
    at(gardener,Y),
    write('The poor man took a blow to the head... there is little enough blood to make him seem like he has merely passed out...'), nl,
    write('...but further inspection has shown that is not the case.'), nl, !.

examine(X) :-
    \+ person(X),
    \+ object(X),
    write('You cannot examine that, please try a different command').


instructions :-
        nl,
        writeln('Commands can be entered into the command prompt in plain english.'),
        writeln('If the command prompt is accidentally exited, you can type <cmd> to reopen it.'),
        writeln('Type <start> to restart the game.'),
        writeln('Type <look> to look around.'),
        writeln('Type <move> to choose a movement direction.'),
        writeln('Type <examine> to choose an item to inspect'),
        writeln('Type <take> and <drop> to choose items to take or drop'),
        writeln('Type <bag> to check the contents of your bag'), nl,
        writeln('Conversations can be held in plain english - you can ask a character about themself, or'), 
        writeln('ask them about another charater or an object.'),
        writeln('Type <bye> to leave any conversation'),nl,
        writeln('Type <help> to view these commands again.').

introduction :-
        writeln('You have been caught in a storm.  The road flooded and now your car has broken down.  Your phone receives no service.'),
        writeln('After following the road for quite some time, you have come across the first house in a long while.'),
        writeln('Completely soaked, you walk down the driveway in hopes the owners will be kind enough to let you call for a tow.'),
        writeln('You notice the garage door is open.  While it would be more tactful to knock on the front door,'),
        writeln('you cant help but think there must be someone inside it if the door has been left open on such a miserable day.'),
        writeln('...'),
        writeln('This is not the sight you expected.'),
        writeln('In front of you, an eldery man is collapsed next to an SUV.'),
        writeln('You get on the ground to see if you can awake the man, but your efforts are in vain.'),
        writeln('The man in front of you is dead.'),
        writeln('Before you can process the gravity of the situation you are in, you hear a shrill scream.'),
        writeln('In front of you, a maid looks at you with a horrified expression.'),
        writeln('The police are quickly called.  They will arrive at the house once the storm clears...'),
        writeln('You must prove your innocence before then!  Good luck!').





/* This rule prints out instructions and tells where you are. */

start :-
        introduction,
        retractall(char_met(_)),
        retractall(at(_, in_bag)),
        assert(marie_gave_key), retract(marie_gave_key),
        assert(guard_asleep), retract(guard_asleep),
        assert(given_drugs), retract(given_drugs),
        assert(key_flag), retract(key_flag),
        assert(homeowner_gave_key), retract(homeowner_gave_key),
        nl,
        writeln('Welcome to Murder Mystery!'),nl,
        writeln('Try entering a command to interact with the game.'),
        writeln('If you cannot figure out an acceptable command, type <help>'),nl,
        get_command.

cmd :-
    get_command.

get_command :-
    writeln('> enter an action: '),nl, 
    readln(Ln),
    handle_command(Ln).


%%%%%%%%%%%%%%%%%%%%%%%%
/* code to handle commands */



%%%%% check instructions
handle_command([X | _]) :-
    member(X, ['instructions', 'help', 'h', 'commands']),
    instructions,
    get_command.

%%%%% look 
handle_command([X | _]) :-
    member(X, ['look', 'look around', 'environment', 'what is around me']),
    look,
    get_command.

%%%%% movement
handle_command([X | _]) :-
    member(X, ['move', 'go', 'enter', 'room', 'leave', 'walk', 'run', 'change room']),
    writeln('Where would you like to move?'),nl,
    i_am_at(Place),
    dir_describe(Place),
    readln(Ln),
    move_helper(Ln),
    get_command.

move_helper([X]) :-
    member(X, ['n', 'north']),
    go(n).
move_helper([X]) :-
    member(X, ['s', 'south']),
    go(s).
move_helper([X]) :-
    member(X, ['e', 'east']),
    go(e).
move_helper([X]) :-
    member(X, ['w', 'west']),
    go(w).

move_helper([X | _]) :-
    member(X, ['nw', 'northwest']),
    go(nw).
move_helper([X | _]) :-
    member(X, ['se', 'southeast']),
    go(se).
move_helper([X | _]) :-
    member(X, ['ne', 'northeast']),
    go(ne).
move_helper([X | _]) :-
    member(X, ['sw', 'southwest']),
    go(sw).

move_helper([X | _]) :-
    i_am_at(Place),
    path(Place, Direction, X),
    go(Direction).

move_helper(_) :-
    writeln('You can''t go in that direction').


%%%%% talk
handle_command([X | _]) :-
    member(X, ['talk', 'talk to', 'speak to', 'speak with', 'converse with']),
    talk_helper,
    get_command.

talk_helper :-
    writeln('Who would you like to talk to? These are the available characters: '),
    i_am_at(Place),
    notice_people_at(Place),
    readln(Ln),
    talk_helper2(Ln).

talk_helper2([X | _]) :-
    person(X),
    at(X, Place),
    i_am_at(Place),
    talk(X).

talk_helper :-
    writeln('Invalid command, please try again.').


%%%%% pick up stuff
handle_command([X | _]) :-
    member(X, ['take', 'pick up', 'grab', 'pick', 'get']),
    writeln('What would you like to take? These are the available objects:'),
    i_am_at(Place),
    notice_objects_at(Place),
    readln(Ln),
    take_object_helper(Ln),
    get_command.

take_object_helper([X | _]) :-
    member(X, ['cancel', 'go back', 'quit', 'undo']).

take_object_helper([X | _]) :-
    take(X).

take_object_helper(_) :-
    writeln('Invalid take command, please try again.').


%%%%% drop stuff
handle_command([X | _]) :-
    member(X, ['drop', 'leave', 'get rid of', 'empty bag', 'trash']),
    writeln('What would you like to drop?'),
    readln(Ln),
    drop_object_helper(Ln),
    get_command.

drop_object_helper([X | _]) :-
    member(X, ['cancel', 'go back', 'quit', 'undo']).

drop_object_helper([X | _]) :-
    drop(X).

drop_object_helper(_) :-
    writeln('Invalid drop command, please try again.').


%%%%% check bag
handle_command(X) :-
    member(X, [['bag'], ['check', 'bag'], ['inventory'], ['check', 'inventory'], ['look', 'in', 'bag'], ['items'], ['holding']]),
    bag,
    get_command.


%%%%% examine things
handle_command(X) :-
    member(X, [['examine'|_], ['inspect'|_], ['look at'|_]]),
    writeln('What would you like to examine?'),
    i_am_at(Place),
    notice_people_at(Place),
    notice_objects_at(Place),
    readln(Ln),
    examine_helper(Ln),
    nl,nl,
    get_command.

examine_helper([X | _]) :-
    examine(X).




%%%%% exit game command prompt
handle_command([X | _]) :-
    member(X, ['exit', 'quit', 'q', 'finish', 'halt']).


%%%%% in case a valid command was not entered:
handle_command(_) :-
    get_command.

%%%%%%%%%%%%%%%%%%%%%%%%



describe(bedroom1) :- 
    dir_describe(bedroom1).
dir_describe(bedroom1) :-
    write('You are in marie''s room.'), nl,
    write('The hallway is to the east.'), nl.

describe(bedroom2) :-
    dir_describe(bedroom2).
dir_describe(bedroom2) :- 
    write('You are in the homeowner''s room.'), nl,
    write('The hallway is to the south.'), nl.

describe(bedroom3) :-
    dir_describe(bedroom3).
dir_describe(bedroom3) :- 
    write('You are in allison''s room.'), nl,
    write('...Allison seems to be missing. Odd.'), nl,
    write('The hallway is to the west.'), nl.

describe(bathroom1) :-
    dir_describe(bathroom1).
dir_describe(bathroom1) :- 
    write('You are in bathroom1'), nl,
    write('The hallway is to the south west.'), nl.

describe(hallway) :- 
    dir_describe(hallway).
dir_describe(hallway) :- 
    write('You are in the hallway'), nl,
    write('The livingroom is to the south'), nl,
    write('the homeowners room is to the north'), nl,
    write('marie''s room is to the west'), nl,
    write('allison''s room is to the east'), nl,
    write('bathroom1 is to the northeast'), nl.

describe(livingroom) :-
    dir_describe(livingroom).
dir_describe(livingroom) :- 
    write('You are in the livingroom'), nl,
    write('The hallway is to the north'), nl,
    write('The basement is to the west'), nl,
    write('bathroom2 is to the south west'), nl,
    write('The kitchen is to the south'), nl,
    write('The patio is to the east'), nl.

describe(basement) :-
    dir_describe(basement).
dir_describe(basement) :- 
    write('You are in the basement'), nl,
    write('The livingroom is to the east'), nl.

describe(bathroom2) :-
    dir_describe(bathroom2).
dir_describe(bathroom2) :- 
    write('You are in bathroom2'), nl,
    write('The livingroom is to the north east.'), nl.

describe(kitchen) :- 
    dir_describe(kitchen).
dir_describe(kitchen) :-
    write('You are in the kitchen.'), nl,
    write('The livingroom is to the north'), nl.

describe(patio) :- 
    at(flashlight, in_bag),
    write('You have exited to the patio.'), nl, nl,
    write('With your flashlight, you were able to examine the area under the furntiture for anything suspicious...'), nl,
    write('and the damage from the storm is even more evident!  The maid seems to have a busy day ahead of her....'), nl, nl,
    write('The livingroom is to the west'), nl,
    write('The garage is to the east'), nl, !.

describe(patio) :- 
    write('You have exited to the patio.'), nl, nl,
    write('The storm still rages on.  The wind and rain have made a mess of the patio furntiture.'), nl,
    write('You should leave before you get too drenched!'), nl, nl,
    write('The livingroom is to the west'), nl,
    write('The garage is to the east'), nl.
dir_describe(patio) :-
    writeln('You are on the patio'),
    write('The livingroom is to the west'), nl,
    write('The garage is to the east'), nl.

describe(garage) :- 
    dir_describe(garage).
dir_describe(garage) :-
    write('You are in the garage.'), nl,
    write('The patio is to the west'), nl.





%===============================================================================
%===============================================================================
%===============================================================================

% Natural language processing code

%===============================================================================
%===============================================================================
%===============================================================================


:- dynamic talk_modifier/2, char_met/1, char_knows/2, marie_gave_key, guard_asleep, given_drugs, key_flag, homeowner_gave_key.
:- discontiguous handle_response/2, char_knows/2, char_thing_info/2.


/* Main talk function */

% attempting to talk to dead gardener
talk(gardener) :-
    write('Corpses make for poor conversation.'), nl.

talk(guard) :-
    guard_asleep,
    write('You could try to talk to him, but hes too fast asleep to hear you.'), nl.


% talking to a character for the first time
talk(X) :-
    person(X),
    at(X, Place),
    i_am_at(Place),
    \+ char_met(X),
    char_intro_speech(X),
    assert(char_met(X)),
    readln(Ln),
    handle_response(Ln, X).

% talking to a character again
talk(X) :-
    person(X),
    at(X, Place),
    i_am_at(Place),
    char_met(X),
    write('What would you like to ask?'), nl,
    readln(Ln),
    handle_response(Ln, X).

talk(X) :-
    \+ person(X),
    write('You can''t talk to that!'), nl.

talk(X) :-
    person(X),
    i_am_at(Place),
    \+ at(X, Place),
    write('They aren''t here'), nl.


/* Code to handle responses */


/* Specific cases - hard code stuff here */
handle_response([Word | _], X) :-
    member(Word, ['goodbye', 'bye', 'leave', 'exit', 'quit', 'finish', 'halt']),
    assert(char_met(X)),
    char_bye_speech(X).

handle_response([who, are, you | _], X) :-
    char_name_info(X),
    talk(X).

handle_response([what, is, your, name | _], X) :-
    char_name_info(X),
    talk(X).


/* Possible ways to ask about something */

handle_response([what, do, you, know, about, the, Thing | _], X) :-
    handle_response([explain, Thing | _], X).
handle_response([what, do, you, know, about, Thing | _], X) :-
    handle_response([explain, Thing | _], X).

handle_response([tell, me, what, you, know, about, the, Thing | _], X) :-
    handle_response([explain, Thing | _], X).
handle_response([tell, me, what, you, know, about, Thing | _], X) :-
    handle_response([explain, Thing | _], X).

handle_response([tell, me, about, the, Thing | _], X) :-
    handle_response([explain, Thing | _], X).
handle_response([tell, me, about, Thing | _], X) :-
    handle_response([explain, Thing | _], X).

handle_response([talk, to, me, about, the, Thing | _], X) :-
    handle_response([explain, Thing | _], X).
handle_response([talk, to, me, about, Thing | _], X) :-
    handle_response([explain, Thing | _], X).
handle_response([talk, about, the, Thing | _], X) :-
    handle_response([explain, Thing | _], X).
handle_response([talk, about, Thing | _], X) :-
    handle_response([explain, Thing | _], X).

handle_response([speak, to, me, about, the, Thing | _], X) :-
    handle_response([explain, Thing | _], X).
handle_response([speak, to, me, about, Thing | _], X) :-
    handle_response([explain, Thing | _], X).
handle_response([speak, about, the, Thing | _], X) :-
    handle_response([explain, Thing | _], X).
handle_response([speak, about, Thing | _], X) :-
    handle_response([explain, Thing | _], X).

handle_response([information, about, the, Thing | _], X) :-
    handle_response([explain, Thing | _], X).
handle_response([information, about, Thing | _], X) :-
    handle_response([explain, Thing | _], X).

handle_response([info, about, the, Thing | _], X) :-
    handle_response([explain, Thing | _], X).
handle_response([info, about, Thing | _], X) :-
    handle_response([explain, Thing | _], X).
handle_response([info, the, Thing | _], X) :-
    handle_response([explain, Thing | _], X).
handle_response([info, Thing | _], X) :-
    handle_response([explain, Thing | _], X).

handle_response([the, Thing, info | _], X) :-
    handle_response([explain, Thing | _], X).
handle_response([Thing, info | _], X) :-
    handle_response([explain, Thing | _], X).

handle_response([explain, the, Thing | _], X) :-
    handle_response([explain, Thing | _], X).



/* Ask a character about an object or person */
% cases where a character is asked about themself
handle_response([explain, Phrase | _], X) :-
    member(Phrase, [you, yourself]),
    char_name_info(X),
    talk(X).
handle_response([explain, X | _], X) :-
    char_name_info(X),
    talk(X).

% case where character doesnt know about the other person
handle_response([explain, OtherPerson | _], X) :-
    person(OtherPerson),
    \+ char_knows(X, OtherPerson),
    write('I don''t know anything about them.'),nl,
    talk(X).

% case where character doesnt know about the object
handle_response([explain, Object | _], X) :-
    object(Object),
    \+ char_knows(X, Object),
    write('I don''t know anything about that.'),nl,
    talk(X).


% case where player asks about something that cant be queried
handle_response([explain, Thing | _], X) :-
    \+ object(Thing),
    \+ person(Thing),
    write('I don''t know what that is.'),nl,
    talk(X).

% case where player asks about an object the npc does know, and is in bag
handle_response([explain, Object | _], X) :-
    at(Object, in_bag),
    char_knows(X, Object),
    char_thing_info(X, Object),
    talk(X).

% case where player asks about an object the npc does know, but NOT in bag
handle_response([explain, Object | _], X) :-
    \+ at(Object, in_bag),
    writeln('You don''t have that item with you.'),
    talk(X).

% case where player asks about another person the npc does know
handle_response([explain, Person | _], X) :-
    char_knows(X, Person),
    char_thing_info(X, Person),
    talk(X).


/* Things that characters know about */



%%%%%%%%%% MADI - SAMPLE TEMPLATES %%%%%%%%%%%%%

%%% template for knowing about object

/* first, initialize that the char knows about the object or person */
/* then create the actual response statement */


%maid 
char_knows(maid, drugs). 
char_thing_info(maid, drugs) :-
    writeln('Yes, those are the multivitamins.... I promise I did not try to harm Allison!'). 
char_knows(maid, wrench). 
char_thing_info(maid, wrench) :-
    writeln('Get that awful thing away from me!').
char_knows(maid, chickendinner). 
char_thing_info(maid, chickendinner) :-
    \+ guard_asleep,
    writeln('Yes, I cooked that for Allison... why are you carrying it around exactly?'). 
char_thing_info(maid, chickendinner) :-
    guard_asleep, \+ given_drugs,
    writeln('W-what?  The food was drugged??? I swear, it was not my doing... I dont know how this could have happened!'),
    writeln('Unless... oh no...  I usually put chopped up multivitamins in Allisons meals… shes a picky eater unfortunately...'),
    writeln('Perhaps theres something wrong with them?  I would never try to hurt one of the girls I swear!'),
    writeln('Here, you can have them.'),
    assert(at(drugs, in_bag)),
    assert(given_drugs).
char_thing_info(maid, chickendinner) :-
    guard_asleep, given_drugs,
    writeln('W-what?  The food was drugged??? I swear, it was not my doing… I dont know how this could have happened!'),
    writeln('Unless… oh no….  I usually put chopped up multivitamins in Allisons meals… shes a picky eater unfortunately...'),
    writeln('Perhaps theres something wrong with them?  I would never try to hurt one of the girls I swear!').

%assistant
char_knows(assistant, wrench). 
char_thing_info(assistant, wrench) :-
    writeln('The murder weapon, I presume?  An awful thing, really.'). 
char_knows(assistant, businesscard). 
char_thing_info(assistant, businesscard) :-
    writeln('Hmm… what is that?  My business card?  Where did you get that?'),
    writeln('...'),
    writeln('Oh, yes, I tutor her every so often.  I must have given her it at some point. ').
char_knows(assistant, ransomnote). 
char_thing_info(assistant, ransomnote) :-
    writeln('!!!!'),
    writeln('How did you find that!!!'),
    writeln('There’s no point hiding it anymore... I tried to kidnap her....'),
    writeln('The gardener caught me as I tried to load her into my car....'),
    writeln('I acted brashly.  In both cases.  I just wanted enough ransom money to get away from'),
    writeln('this god-awful job.  Ill turn myself in.'),
    writeln('YOU PROVED YOURSELF INNOCENT, WINNING THE GAME.'),
    writeln('Type `halt` to end the game.').

    %halt here



%homeowner
char_knows(homeowner, wrench). 
char_thing_info(homeowner, wrench) :-
    writeln('The murder weapon, I presume?  An awful thing, really.').
char_knows(homeowner, businesscard). 
char_thing_info(homeowner, businesscard) :-
    \+ key_flag,
    writeln('Oh, thats my assistants card.  Kind of ugly looking, dont you think?').
char_thing_info(homeowner, businesscard) :-
    key_flag, \+ homeowner_gave_key,
    writeln('My assistant...?  He works out of the basement... perhaps you will find something there.'),
    writeln('Take the basementkey.'),
    assert(at(basementkey, in_bag)),
    assert(homeowner_gave_key).
char_thing_info(homeowner, businesscard) :-
    key_flag, homeowner_gave_key,
    writeln('My assistant...?  He works out of the basement... perhaps you will find something there.').
char_knows(homeowner, ransomnote). 
char_thing_info(homeowner, ransomnote) :-
    writeln('What in the world??? Show the assistant this.  Make him confess his crimes.').
char_knows(homeowner, drugs). 
char_thing_info(homeowner, drugs) :-
    writeln('Allison is missing?  And you suspect she may have been drugged and kidnapped???'),
    writeln('This is... unbelievable!  Do you have any idea who is behind this?'),
    assert(key_flag).


%marie
char_knows(marie, ribbon).
char_thing_info(marie, ribbon) :-
    \+ marie_gave_key,
    writeln('Thats my sisters ribbon... do you need to ask her something?'),
    writeln('She likes to keep her room locked, so ill give you the key'),
    assert(at(bedroomkey, in_bag)),
    assert(marie_gave_key).
char_thing_info(marie, ribbon) :-
    marie_gave_key,
    writeln('That''s my sister''s ribbon.').
char_knows(marie, wrench).
char_thing_info(marie, wrench) :-
    writeln('Oh gosh, is that the murder weapon!!!').
char_knows(marie, businesscard).
char_thing_info(marie, businesscard) :-
    writeln('Oh, thats the business card for my dads assistant.').
char_knows(marie, bedroomkey).
char_thing_info(marie, bedroomkey) :-
    writeln('Thats the key to Allisons room that I gave you!').

%allison
char_knows(allison, wrench).
char_thing_info(allison, wrench) :-
    writeln('Oh gosh, is that blood! Is everyone OK?!').
char_knows(allison, ribbon).
char_thing_info(allison, ribbon) :-
    writeln('Thats my ribbon... did I drop it somewhere.... My head is fuzzy....').
char_knows(allison, chickendinner).
char_thing_info(allison, chickendinner) :-
    writeln('Thats my dinner... I think I fell asleep after eating a bit of it....').

%guard
char_knows(guard, wrench).
char_thing_info(guard, wrench) :-
    writeln('Agh, the murder weapon.  Real nasty').
char_knows(guard, chickendinner).
char_thing_info(guard, chickendinner) :-
    writeln('Nice, I love chicken, give me a piece!'),
    writeln('*The guard passes out in front of you*'),
    assert(guard_asleep).


%%% template for knowing about person

%maid
char_knows(maid, gardener).
char_thing_info(maid, gardener) :-
    writeln('The poor man.  He was so sweet... I dont know why anyone would hurt him.').
char_knows(maid, assistant).
char_thing_info(maid, assistant) :-
    writeln('Oh, I dont know much of the assistant.  He seems overworked though, haha.  I mean, moreso than the rest of us.').
char_knows(maid, homeowner).
char_thing_info(maid, homeowner) :-
    writeln('My boss, huh, well, he pays me... and I appreciate that.  Prefer his kids though.').
char_knows(maid, marie).
char_thing_info(maid, marie) :-
    writeln('Marie is a sweetheart.').
char_knows(maid, allison).
char_thing_info(maid, allison) :-
    writeln('Allison is a nice girl, but kind of reclusive... and a picky eater... makes my job difficult.').
char_knows(maid, guard).
char_thing_info(maid, guard) :-
    writeln('Hes kind of an oaf... does his job well enough though....').

%assistant
char_knows(assistant, gardener).
char_thing_info(assistant, gardener) :-
    writeln('Ah... the old man didnt deserve to go this way... he was a very hardworking and likeable fellow.').
char_knows(assistant, maid).
char_thing_info(assistant, maid) :-
    writeln('She cooks and cleans for the entire household.  Doesnt get paid enough, honestly.').
char_knows(assistant, homeowner).
char_thing_info(assistant, homeowner) :-
    writeln('Haha... I do care for the paycheque but I cant say I care for the man').
char_knows(assistant, marie).
char_thing_info(assistant, marie) :-
    writeln('A bright young lady... if only her father had her kindness.').
char_knows(assistant, allison).
char_thing_info(assistant, allison) :-
    writeln('A little bit of a recluse, that one... but very sweet, like her sister.').
char_knows(assistant, guard).
char_thing_info(assistant, guard) :-
    writeln('A bit of a brute, that one....').

%homeowner
char_knows(homeowner, gardener).
char_thing_info(homeowner, gardener) :-
    writeln('Poor sod.  Hard working, nice fellow.  Complained less than the rest of them too.').
char_knows(homeowner, maid).
char_thing_info(homeowner, maid) :-
    writeln('Shes kind of ditsy, but she does her job well enough.  My wife cooked better when she was around though....').
char_knows(homeowner, assistant).
char_thing_info(homeowner, assistant) :-
    writeln('Winy.  Pathetic.  Havent been able to find a better replacement.').
char_knows(homeowner, marie).
char_thing_info(homeowner, marie) :-
    writeln('My elder daughter... a little outspoken, but brats will be brats.').
char_knows(homeowner, allison).
char_thing_info(homeowner, allison) :-
    writeln('My younger daughter... very inward, that one.').
char_knows(homeowner, guard).
char_thing_info(homeowner, guard) :-
    writeln('Probably going to be fired... a guard who allows a murder to happen... really.....').

%marie
char_knows(marie, gardener).
char_thing_info(marie, gardener) :-
    writeln('He was such a nice guy....').
char_knows(marie, maid).
char_thing_info(marie, maid) :-
    writeln('Shes such a good cook!  Funny too').
char_knows(marie, assistant).
char_thing_info(marie, assistant) :-
    writeln('He always looks so tired....').
char_knows(marie, homeowner).
char_thing_info(marie, homeowner) :-
    writeln('Oh dad, hes... a special guy').
char_knows(marie, allison).
char_thing_info(marie, allison) :-
    writeln('My little sister?  Shes kind of shy, but really sweet.').
char_knows(marie, guard).
char_thing_info(marie, guard) :-
    writeln('Hahah hes such a funny guy.').

%allison
char_knows(allison, gardener).
char_thing_info(allison, gardener) :-
    writeln('Im kind of tired... can you stop asking me about people').
char_knows(allison, maid).
char_thing_info(allison, maid) :-
    writeln('Im kind of tired... can you stop asking me about people').
char_knows(allison, assistant).
char_thing_info(allison, assistant) :-
    writeln('Im kind of tired... can you stop asking me about people').
char_knows(allison, homeowner).
char_thing_info(allison, homeowner) :-
    writeln('Im kind of tired... can you stop asking me about people').
char_knows(allison, marie).
char_thing_info(allison, marie) :-
    writeln('Im kind of tired... can you stop asking me about people').
char_knows(allison, guard).
char_thing_info(allison, guard) :-
    writeln('Im kind of tired... can you stop asking me about people').

%guard
char_knows(guard, gardener).
char_thing_info(guard, gardener) :-
    writeln('Poor guy.  Nice fellow. Laughed at my jokes.').
char_knows(guard, maid).
char_thing_info(guard, maid) :-
    writeln('A bit snappy, but her chicken is to die for!').
char_knows(guard, assistant).
char_thing_info(guard, assistant) :-
    writeln('Stuck up guy.  Hes probably just overworked though, hahahahaha.').
char_knows(guard, homeowner).
char_thing_info(guard, homeowner) :-
    writeln('Moody guy.  Could pay better.').
char_knows(guard, marie).
char_thing_info(guard, marie) :-
    writeln('Marie is a sweetheart!  A lot easier to talk to then her sister.').
char_knows(guard, allison).
char_thing_info(guard, allison) :-
    writeln('Quiet girl.  She seems nice though.').


%%%%%%%%%%%%%%%%%%%%%%%%


%%% Add additional things characters know here, based on these templates









%%%%%%%%%%%%%%%%%%%%%%%%


/* non-specific cases */


% case where beginning of line cannot be parsed
handle_response([_| Y], X) :-
    handle_response(Y, X).

% case where all other handle_response queries failed
handle_response(_, X) :-
    write('I''m sorry, I don''t understand what you mean.'), nl,
    readln(Ln),
    handle_response(Ln, X).



/* Intro speeches for each character */

char_intro_speech(maid) :-
    write('Oh, its you... You know, you said you didnt murder him but im not so sure....'), nl.
char_intro_speech(assistant) :-
    write('Ah, the supposed murderer.  Good luck proving yourself innocent.'), nl.
char_intro_speech(homeowner) :-
    write('Make yourself comfortable... the police should be here for questioning as soon as the storm clears.'), nl.
char_intro_speech(marie) :-
    write('Oh hello, and who might you be?'), nl.
char_intro_speech(allison) :-
    write('Where am I... whats going on...  who are you?'), nl.
char_intro_speech(guard) :-
    write('Oh, its the troublemaker... '), nl.

/* Farewell speeches for each character */

char_bye_speech(maid) :-
    write('Oh, thats all?'), nl.
char_bye_speech(assistant) :-
    write('Farewell.'), nl.
char_bye_speech(homeowner) :-
    write('Very well.'), nl.
char_bye_speech(marie) :-
    write('Until next time!'), nl.
char_bye_speech(allison) :-
    write('...bye'), nl.
char_bye_speech(guard) :-
    write('Goodbye!'), nl.

/* Name and basic info speech for each character */

char_name_info(maid) :-
    write('Oh me?  I do the cooking and cleaning around here.'), nl.
char_name_info(assistant) :-
    write('Me?  I handle extra business manners for my boss` company.'), nl.
char_name_info(homeowner) :-
    write('This is my estate.  I live here with my two daughters and live-in help.'), nl.
char_name_info(marie) :-
    write('My name is marie.  I am the older daughter of the household.  Its nice to meet you.'), nl.
char_name_info(allison) :-
    write('...my name is allison....'), nl.
char_name_info(guard) :-
    write('Oh, me?  Im in charge of the security around here.... We`ll see for how long after the gardener though!'), nl,
    write('...too soon?'), nl.