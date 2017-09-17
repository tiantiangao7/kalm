/*
Usage: get_target_verb(+Conditions,+Target,-VerbPredicate)
*/
get_target_verb(Conditions,Target,VerbPredicate) :-
    basics:member(predicate(V1,V,V2,V3)-V4/V5,Conditions),
    V == Target,
    VerbPredicate = predicate(V1,V,V2,V3)-V4/V5.

get_target_verb(Conditions,Target,VerbPredicate) :-
    basics:member(predicate(V1,V,V2)-V3/V4,Conditions),
    V == Target,
    VerbPredicate = predicate(V1,V,V2)-V3/V4.

get_target_verb(Conditions,Target,VerbPredicate) :-
    basics:member(predicate(V1,V,V2,V3,V4)-V5/V6,Conditions),
    V == Target,
    VerbPredicate = predicate(V1,V,V2,V3,V4)-V5/V6.

/*
Usage: get_ext_from_target_verb(+Conditions,+TargetPredicate,-Ext)
Example: the physicians(ext) performed(target) the surgery.
*/
get_ext_from_target_verb(Conditions,TargetPredicate,Ext) :-
    TargetPredicate = predicate(_,_,V2)-_/_,
    get_noun_lexicon_from_var(V2,Conditions,Ext).

get_ext_from_target_verb(Conditions,TargetPredicate,Ext) :-
    TargetPredicate = predicate(_,_,V2,_)-_/_,
    get_noun_lexicon_from_var(V2,Conditions,Ext).

get_ext_from_target_verb(Conditions,TargetPredicate,Ext) :-
    TargetPredicate = predicate(_,_,V2,_,_)-_/_,
    get_noun_lexicon_from_var(V2,Conditions,Ext).

/*
Usage: get_obj_from_target_verb(+Conditions,+TargetPredicate,-Obj)
Example: 1. Voters approved(target) the plan (obj).
         2. John gives a cup(measuring noun) of water(dobj) to Mary(idobj).
         3. John gives Mary(idobj) some candy(dobj).
*/
get_obj_from_target_verb(Conditions,TargetPredicate,Obj) :-
    TargetPredicate = predicate(_,V,_,V3)-_/_,
    not motion_verb(V),
    get_noun_lexicon_from_var(V3,Conditions,Obj),
    not measuring_noun(Obj).

get_obj_from_target_verb(Conditions,TargetPredicate,Obj) :-
    TargetPredicate = predicate(_,V,_,V3,_)-_/_,
    not motion_verb(V),
    get_noun_lexicon_from_var(V3,Conditions,Obj),
    not measuring_noun(Obj).

get_obj_from_target_verb(Conditions,TargetPredicate,Obj) :-
    TargetPredicate = predicate(_,V,_,V3)-_/_,
    not motion_verb(V),
    var(V3),
    basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
    V3 == W1,
    measuring_noun(W),
    basics:member(relation(U1,of,U2)-U3/U4,Conditions),
    W1 == U1,
    get_noun_lexicon_from_var(U2,Conditions,Obj).

get_obj_from_target_verb(Conditions,TargetPredicate,Obj) :-
    TargetPredicate = predicate(_,V,_,V3,_)-_/_,
    not motion_verb(V),
    var(V3),
    basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
    V3 == W1,
    measuring_noun(W),
    basics:member(relation(U1,of,U2)-U3/U4,Conditions),
    W1 == U1,
    get_noun_lexicon_from_var(U2,Conditions,Obj).

/*
Usage: get_dep_from_target_verb(+Conditions,+TargetPredicate,+PP,-Dep)
Example: 1. Mary ordered a steak in a restaurant (pp).
         2. Mary runs 10 miles(pp) in a day(pp).
         3. Mary gives John a candy(pp).  
*/
get_dep_from_target_verb(Conditions,TargetPredicate,PP,Dep) :-
    TargetPredicate = predicate(W1,W,W2)-W3/W4,
    basics:member(modifier_pp(V1,V,V2)-V3/V4,Conditions),
    V == PP,
    V1 == W1,
    get_noun_lexicon_from_var(V2,Conditions,Dep).

get_dep_from_target_verb(Conditions,TargetPredicate,PP,Dep) :-
    TargetPredicate = predicate(W1,W,W2,W3)-W4/W5,
    basics:member(modifier_pp(V1,V,V2)-V3/V4,Conditions),
    V == PP,
    V1 == W1,
    get_noun_lexicon_from_var(V2,Conditions,Dep).

get_dep_from_target_verb(Conditions,TargetPredicate,PP,Dep) :-
    TargetPredicate = predicate(W1,W,W2,W3,W4)-W5/W6,
    basics:member(modifier_pp(V1,V,V2)-V3/V4,Conditions),
    V == PP,
    V1 == W1,
    get_noun_lexicon_from_var(V2,Conditions,Dep).

get_dep_from_target_verb(Conditions,TargetPredicate,Dep) :-
    TargetPredicate = predicate(V1,V,V2,V3)-V4/V5,
    get_noun_lexicon_from_var(V3,Conditions,Dep),
    measuring_noun(Dep).

get_dep_from_target_verb(Conditions,TargetPredicate,Dep) :-
    TargetPredicate = predicate(V1,V,V2,V3,V4)-V5/V6,
    get_noun_lexicon_from_var(V4,Conditions,Dep),
    not measuring_noun(Dep).

get_dep_from_target_verb(Conditions,TargetPredicate,Dep) :-
    TargetPredicate = predicate(V1,V,V2,V3,V4)-V5/V6,
    var(V4),
    basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
    V4 == W1,
    measuring_noun(W),
    basics:member(relation(U1,of,U2)-U3/U4,Conditions),
    W1 == U1,
    get_noun_lexicon_from_var(U2,Conditions,Dep).

get_dep_from_target_verb(Conditions,TargetPredicate,here) :-
    TargetPredicate = predicate(V1,V,V2)-V3/V4,
    basics:member(modifier_adv(W1,here,W2)-W3/W4,Conditions),
    V1 == W1.

get_dep_from_target_verb(Conditions,TargetPredicate,here) :-
    TargetPredicate = predicate(V1,V,V2,V3)-V4/V6,
    basics:member(modifier_adv(W1,here,W2)-W3/W4,Conditions),
    V1 == W1.

get_dep_from_target_verb(Conditions,TargetPredicate,here) :-
    TargetPredicate = predicate(V1,V,V2,V3,V4)-V5/V6,
    basics:member(modifier_adv(W1,here,W2)-W3/W4,Conditions),
    V1 == W1.

/*
Usage: get_target_adj(+Conditions,+Target,-AdjPredicate)
*/
get_target_adj(Conditions,Target,AdjPredicate) :-
    basics:member(property(W1,W,W2)-W3/W4,Conditions),
    W == Target,
    AdjPredicate = property(W1,W,W2)-W3/W4.

get_target_adj(Conditions,Target,AdjPredicate) :-
    basics:member(property(W1,W,W2,W3)-W4/W5,Conditions),
    W == Target,
    AdjPredicate = property(W1,W,W2,W3)-W4/W5.

get_target_adj(Conditions,Target,AdjPredicate) :-
    basics:member(property(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
    W == Target,
    AdjPredicate = property(W1,W,W2,W3,W4,W5)-W6/W7.

/*
Usage: get_ext_from_target_adj(+Conditions,+TargetPredicate,-Ext)
Example: the house(ext) is red(target).
*/
get_ext_from_target_adj(Conditions,TargetPredicate,Ext) :-
    TargetPredicate = property(W1,W,W2,W3)-W4/W5,
    basics:member(predicate(V1,be,V2,V3)-V4/V5,Conditions),
    W1 == V3,
    get_noun_lexicon_from_var(V2,Conditions,Ext).

get_ext_from_target_adj(Conditions,TargetPredicate,Ext) :-
    TargetPredicate = property(W1,W,W2,W3,W4,W5)-W6/W7,
    basics:member(predicate(V1,be,V2,V3)-V4/V5,Conditions),
    W1 == V3,
    get_noun_lexicon_from_var(V2,Conditions,Ext).

/*
Usage: get_head_from_target_adj(+Conditions,+TargetPredicate,-Head)
Example: the small(target) children(head)
*/
get_head_from_target_adj(Conditions,TargetPredicate,Head) :-
    TargetPredicate = property(W1,W,W2)-W3/W4,
    get_noun_lexicon_from_var(W1,Conditions,Head).

get_head_from_target_adj(Conditions,TargetPredicate,Head) :-
    TargetPredicate = property(W1,W,W2,W3)-W4/W5,
    get_noun_lexicon_from_var(W1,Conditions,Head).

get_head_from_target_adj(Conditions,TargetPredicate,Head) :-
    TargetPredicate = property(W1,W,W2,W3,W4,W5)-W6/W7,
    get_noun_lexicon_from_var(W1,Conditions,Head).

/*
Usage: get_dep_from_target_adj(+Conditions,+TargetPredicate,-Dep)
Example: 1. John is fond-of Mary.
         2. John is married to Mary.
*/
get_dep_from_target_adj(Conditions,TargetPredicate,Dep) :-
    TargetPredicate = property(W1,W,W2,W3)-W4/W5,
    get_noun_lexicon_from_var(W3,Conditions,Dep).

get_dep_from_target_adj(Conditions,TargetPredicate,Dep) :-
    TargetPredicate = property(W1,W,W2,W3)-W4/W5,
    basics:member(predicate(V1,V,V2,V3)-V4/V5,Conditions),
    V3 == W1,
    basics:member(modifier_pp(U1,U,U2)-U3/U4,Conditions),
    U1 == V1,
    get_noun_lexicon_from_var(U2,Conditions,Dep).

/*
Usage: get_target_preposition(+Conditions,+Target,-PPPredicate)
*/
get_target_preposition(Conditions,Target,PPPredicate) :-
    basics:member(modifier_pp(X1,X,X2),Conditions),
    X == Target,
    PPPredicate = modifier_pp(X1,X,X2).

get_target_preposition(Conditions,of,PPPredicate) :-
    basics:member(relation(X1,of,X2),Conditions),
    PPPredicate = relation(X1,of,X2).

/*
Usage: get_ext_from_target_preposition(+Conditions,+TargetPredicate,-Ext)
Example: 1. The ball(ext) is under(target) the table.
         2. a man(ext) of(target) 30
*/

get_ext_from_target_preposition(Conditions,TargetPredicate,Ext) :-
    TargetPredicate = modifier_pp(X1,X,X2),
    basics:member(predicate(W1,be,W2)-W3/W4,Conditions),
    W1 == X1,
    get_noun_lexicon_from_var(W2,Conditions,Ext).

get_ext_from_target_preposition(Conditions,TargetPredicate,Ext) :-
    TargetPredicate = relation(X1,of,X2),
    get_noun_lexicon_from_var(X1,Conditions,Ext).

/*
Usage: get_obj_from_target_preposition(+Conditions,+TargetPredicate,-Obj)
Example: 1. We have a a glass of wine before(target) the meal(obj).
         2. a man of(target) 20(obj)
*/
get_obj_from_target_preposition(Conditions,TargetPredicate,Obj) :-
    TargetPredicate = modifier_pp(X1,X,X2),
    get_noun_lexicon_from_var(X2,Conditions,Obj).

get_obj_from_target_preposition(Conditions,TargetPredicate,Obj) :-
    TargetPredicate = relation(X1,of,X2),
    get_noun_lexicon_from_var(X2,Conditions,Obj).

/*
Usage: get_target_noun(+Conditions,+Target,-NounPredicate)
*/
get_target_noun(Conditions,Target,NounPredicate) :-
    basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
    W == Target,
    NounPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7.

/*
Usage: get_dep_from_target_noun(+Conditions,+TargetPrediate,-Dep)
Example: 1. The letter(target) is to the president(dep).
         2. Mary(target) is a nurse(dep).
         3. an accomplishment(target) of a project(dep)
         4. a sleeeping(dep) cat(target)
*/

get_dep_from_target_noun(Conditions,TargetPrediate,Dep) :-
    TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7,
    basics:member(predicate(V1,be,V2)-V3/V4,Conditions),
    V2 == W1,
    basics:member(modifier_pp(X1,X,X2),Conditions),
    X1 == V1,
    get_noun_lexicon_from_var(X2,Conditions,Dep).

get_dep_from_target_noun(Conditions,TargetPrediate,Dep) :-
    TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7,
    basics:member(predicate(V1,be,V2,V3)-V4/V5,Conditions),
    V2 == W1,
    get_noun_lexicon_from_var(V3,Conditions,Dep).

get_dep_from_target_noun(Conditions,TargetPrediate,Dep) :-
    TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7,
    basics:member(relation(U1,of,U2)-U3/U4,Conditions),
    U1 == W1,
    get_noun_lexicon_from_var(U2,Conditions,Dep).

get_dep_from_target_noun(Conditions,TargetPrediate,Dep) :-
    TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7,
    basics:member(property(U1,U,U2)-U3/U4,Conditions),
    U1 == W1,
    get_noun_lexicon_from_var(U,Conditions,Dep).

get_dep_from_target_noun(Conditions,TargetPrediate,Dep) :-
    TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7,
    basics:member(property(U1,U,U2,U3)-U4/U5,Conditions),
    U1 == W1,
    get_noun_lexicon_from_var(U,Conditions,Dep).

get_dep_from_target_noun(Conditions,TargetPrediate,Dep) :-
    TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7,
    basics:member(property(U1,U,U2,U3,U4,U5)-U6/U7,Conditions),
    U1 == W1,
    get_noun_lexicon_from_var(U,Conditions,Dep).

/*
Usage: get_ext_from_target_noun(+Conditions,+TargetPrediate,-Ext)
Example: 1. Mary(ext) makes a statement(target) to the press.
         2. Mary(ext) takes a bath(target) for an hour.
         3. Mary's(ext) abandonment of a project(target)
         4. Mary is at an age of 20.
*/
get_ext_from_target_noun(Conditions,TargetPrediate,Ext) :-
    TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7,
    basics:member(predicate(V1,V,V2,V3)-V4/V5,Conditions),
    V3 == W1,
    get_noun_lexicon_from_var(V2,Conditions,Ext).

get_ext_from_target_noun(Conditions,TargetPrediate,Ext) :-
    TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7,
    basics:member(predicate(V1,V,V2,V3,V4)-V5/V6,Conditions),
    V3 == W1,
    get_noun_lexicon_from_var(V2,Conditions,Ext).

get_ext_from_target_noun(Conditions,TargetPrediate,Ext) :-
    TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7,
    basics:member(relation(V1,of,V2)-V3/V4,Conditions),
    V2 == W1,
    basics:member(relation(T1,of,T2)-T3/T4,Conditions),
    T1 == V1,
    T2 \= V2,
    get_noun_lexicon_from_var(T2,Conditions,Ext).

get_ext_from_target_noun(Conditions,TargetPrediate,Ext) :-
    TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7,
    basics:member(modifier_pp(V1,V,V2)-V3/V4,Conditions),
    V2 == W1,
    basics:member(predicate(U1,U,U2)-U3/U4,Conditions),
    U1 == V1,
    get_noun_lexicon_from_var(U2,Conditions,Ext).

get_ext_from_target_noun(Conditions,TargetPrediate,Ext) :-
    TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7,
    basics:member(modifier_pp(V1,V,V2)-V3/V4,Conditions),
    V2 == W1,
    basics:member(predicate(U1,U,U2,U3)-U4/U5,Conditions),
    U1 == V1,
    get_noun_lexicon_from_var(U2,Conditions,Ext).

get_ext_from_target_noun(Conditions,TargetPrediate,Ext) :-
    TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7,
    basics:member(modifier_pp(V1,V,V2)-V3/V4,Conditions),
    V2 == W1,
    basics:member(predicate(U1,U,U2,U3,U4)-U5/U6,Conditions),
    U1 == V1,
    get_noun_lexicon_from_var(U2,Conditions,Ext).

/*
Usage: get_gen_from_target_noun(+Conditions,+TargetPrediate,-Gen)
Example: Mary's(gen) father(target)
*/
get_gen_from_target_noun(Conditions,TargetPrediate,Gen) :-
    TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7,
    basics:member(relation(V1,of,V2)-V3/V4,Conditions),
    V1 == W1,
    get_noun_lexicon_from_var(V2,Conditions,Gen).

/*
Usage: get_quant_from_target_noun(+Conditions,+TargetPrediate,-Quant)
Example: Mary buys 3 kg(quant) of water(target).
*/
get_quant_from_target_noun(Conditions,TargetPrediate,Quant) :-
    TargetPredicate = object(W1,W,W2,W3,W4,Quant)-W6/W7.

/*
World knowledge
*/
motion_verb(ride).
motion_verb(fly).
motion_verb(bike).
motion_verb(take).

measuring_noun(ton). 
measuring_noun(dollar). 

/*
Helper functions
*/
get_noun_lexicon_from_var(Arg,Conditions,Noun) :- 
    var(Arg),!,
    basics:member(object(V1,V,V2,V3,V4,V5)-V6/V7,Conditions),
    Arg == V1,
    Noun = V.

get_noun_lexicon_from_var(named(X),_,X).
get_noun_lexicon_from_var(string(X),_,X).
get_noun_lexicon_from_var(int(X),_,X).
