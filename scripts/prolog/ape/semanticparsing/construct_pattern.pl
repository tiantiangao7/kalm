:- table reach/5.
:- import append/3, member/2 from basics.
/*
Usage: construct_lfe(+DRSFacts,+Index1,+Index2,-Output)
*/
construct_lfe(DRSFacts,TargetIndex,FEIndex,LFE) :-
    TargetIndex \= FEIndex,
    reach(DRSFacts,TargetIndex,FEIndex,LFE,_).    

construct_lfe(_,TargetIndex,TargetIndex,['self']).

/*
Usage: get_predicate_from_word_index(+DRSFacts,+Index,-WordPredicate)
*/
get_predicate_from_word_index(DRSFacts,Index,Predicate-Index) :- 
    member(Predicate-Index,DRSFacts).

/*
Usage: get_predicate_type(+Predicate,-Type)
*/
get_predicate_type(Predicate,Type) :-
    Predicate = Main-_,
    Main =.. [Type|_].

/*
Usage: check_pp_predicate(+Predicate,+PPWord)
*/
check_pp_predicate(modifier_pp(_,PPWord,_)-_,PPWord).

check_pp_predicate(modifier_pp(_,loc,_)-_,in).
check_pp_predicate(modifier_pp(_,loc,_)-_,at).
check_pp_predicate(modifier_pp(_,loc,_)-_,on).
check_pp_predicate(modifier_pp(_,loc,_)-_,under).
check_pp_predicate(modifier_pp(_,loc,_)-_,within).
check_pp_predicate(modifier_pp(_,loc,_)-_,to).
check_pp_predicate(modifier_pp(_,loc,_)-_,toward).
check_pp_predicate(modifier_pp(_,loc,_)-_,above).
check_pp_predicate(modifier_pp(_,loc,_)-_,across).
check_pp_predicate(modifier_pp(_,loc,_)-_,down).
check_pp_predicate(modifier_pp(_,loc,_)-_,near).
check_pp_predicate(modifier_pp(_,loc,_)-_,outside).
check_pp_predicate(modifier_pp(_,loc,_)-_,upon).
check_pp_predicate(modifier_pp(_,loc,_)-_,along).
check_pp_predicate(modifier_pp(_,loc,_)-_,over).
check_pp_predicate(modifier_pp(_,loc,_)-_,from).

check_pp_predicate(modifier_pp(_,time,_)-_,in).
check_pp_predicate(modifier_pp(_,time,_)-_,on).
check_pp_predicate(modifier_pp(_,time,_)-_,near).
check_pp_predicate(modifier_pp(_,time,_)-_,around).
check_pp_predicate(modifier_pp(_,time,_)-_,until).
check_pp_predicate(modifier_pp(_,time,_)-_,before).
check_pp_predicate(modifier_pp(_,time,_)-_,after).
check_pp_predicate(modifier_pp(_,time,_)-_,till).
check_pp_predicate(modifier_pp(_,time,_)-_,from).

/*
Usage: reach(+DRSFacts,+I1,+I2,-PathList,-NodeList)
*/
reach(DRSFacts,I1,I2,Path,[I1,Arg,I2]) :- 
    get_predicate_from_word_index(DRSFacts,I1,P1),
    get_predicate_type(P1,Type),
    get_dependency(DRSFacts,Type,P1,Arg,I2,Path).

reach(DRSFacts,I1,I3,PathList2,NodeList2) :- 
    reach(DRSFacts,I1,I2,PathList,NodeList),
    get_predicate_from_word_index(DRSFacts,I2,P2),
    get_predicate_type(P2,Type),
    get_dependency(DRSFacts,Type,P2,Arg,I3,Path),
    not member(Arg,NodeList),
    not member(I3,NodeList),
    append(NodeList,[Arg,I3],NodeList2),
    append(PathList,Path,PathList2).
    
/*    
Usage: get_dependency(+DRSFacts,verb,+P1,-I2)
*/     
get_dependency(DRSFacts,predicate,P1,Arg,I2,['verb->subject']) :-
    get_subject_from_verb(DRSFacts,P1,Arg,I2).
    
get_dependency(DRSFacts,predicate,P1,Arg,I2,['verb->object']) :-
    get_object_from_verb(DRSFacts,P1,Arg,I2).

get_dependency(DRSFacts,predicate,P1,Arg,I2,['verb->iobject']) :-
    get_iobject_from_verb(DRSFacts,P1,Arg,I2).

get_dependency(DRSFacts,predicate,P1,Arg,I2,['verb->pp']) :-
    get_pp_from_verb(DRSFacts,P1,Arg,I2).

get_dependency(DRSFacts,predicate,P1,Arg,I2,['verb->adv']) :-
    get_adv_from_verb(DRSFacts,P1,Arg,I2).
    
get_dependency(DRSFacts,modifier_pp,P1,Arg,I2,[pp_constraint(Preposition,'pp->dep')]) :-
    get_dep_from_pp(DRSFacts,P1,Arg,I2),
    P1 = Predicate-_,
    Predicate =.. [_,_,Preposition,_].
    
get_dependency(DRSFacts,modifier_pp,P1,Arg,I2,[pp_constraint(Preposition,'pp->verb')]) :-
    get_verb_from_pp(DRSFacts,P1,Arg,I2),
    P1 = Predicate-_,
    Predicate =.. [_,_,Preposition,_].    

get_dependency(DRSFacts,modifier_adv,P1,Arg,I2,['adv->verb']) :-
    get_verb_from_adv(DRSFacts,P1,Arg,I2).    

get_dependency(DRSFacts,predicate,P1,Arg,I2,['verb->adj']) :-
    get_adj_from_verb(DRSFacts,P1,Arg,I2).
    
get_dependency(DRSFacts,property,P1,Arg,I2,['adj->pobject']) :-
    get_pobject_from_adj(DRSFacts,P1,Arg,I2).

get_dependency(DRSFacts,property,P1,Arg,I2,['adj->sobject']) :-
    get_sobject_from_adj(DRSFacts,P1,Arg,I2).
    
get_dependency(DRSFacts,property,P1,Arg,I2,['adj->tobject']) :-
    get_tobject_from_adj(DRSFacts,P1,Arg,I2).
    
get_dependency(DRSFacts,property,P1,Arg,I2,['adj->verb']) :-
    get_verb_from_adj(DRSFacts,P1,Arg,I2).
    
get_dependency(DRSFacts,relation,P1,Arg,I2,['rel->lobject']) :-
    get_lobject_from_rel(DRSFacts,P1,Arg,I2).
    
get_dependency(DRSFacts,relation,P1,Arg,I2,['rel->robject']) :-
    get_robject_from_rel(DRSFacts,P1,Arg,I2).

get_dependency(DRSFacts,predicate,P1,Arg,I2,['verb->rel']) :-
    get_rel_from_verb(DRSFacts,P1,Arg,I2).
    
get_dependency(DRSFacts,object,P1,Arg,I2,['subject->verb']) :-
    get_verb_from_subject(DRSFacts,P1,Arg,I2).
    
get_dependency(DRSFacts,object,P1,Arg,I2,['object->verb']) :-
    get_verb_from_object(DRSFacts,P1,Arg,I2).
    
get_dependency(DRSFacts,object,P1,Arg,I2,['iobject->verb']) :-
    get_verb_from_iobject(DRSFacts,P1,Arg,I2).
    
get_dependency(DRSFacts,object,P1,Arg,I2,['dep->pp']) :-
    get_pp_from_dep(DRSFacts,P1,Arg,I2).
    
get_dependency(DRSFacts,object,P1,Arg,I2,['lobject->rel']) :-
    get_rel_from_lobject(DRSFacts,P1,Arg,I2).
    
get_dependency(DRSFacts,object,P1,Arg,I2,['robject->rel']) :-
    get_rel_from_robject(DRSFacts,P1,Arg,I2).
    
get_dependency(DRSFacts,object,P1,Arg,I2,['pobject->adj']) :-
    get_adj_from_pobject(DRSFacts,P1,Arg,I2).

get_dependency(DRSFacts,object,P1,Arg,I2,['sobject->adj']) :-
    get_adj_from_sobject(DRSFacts,P1,Arg,I2). 
    
get_dependency(DRSFacts,object,P1,Arg,I2,['tobject->adj']) :-
    get_adj_from_tobject(DRSFacts,P1,Arg,I2).       

get_dependency(DRSFacts,relation,P1,Arg,I2,['rel->pp']) :-
    get_pp_from_rel(DRSFacts,P1,Arg,I2).       

get_dependency(DRSFacts,modifier_pp,P1,Arg,I2,[pp_constraint(Preposition,'pp->rel')]) :-
    get_rel_from_pp(DRSFacts,P1,Arg,I2),
    P1 = Predicate-_,
    Predicate =.. [_,_,Preposition,_].

get_dependency(DRSFacts,modifier_pp,P1,Arg,I2,[pp_constraint(Preposition,'pp->pp')]) :-
    get_pp_from_pp(DRSFacts,P1,Arg,I2),
    P1 = Predicate-_,
    Predicate =.. [_,_,Preposition,_].

get_dependency(DRSFacts,object,P1,Arg,I2,['has_part->object']) :-
    get_object_from_has_part(DRSFacts,P1,Arg,I2).

get_dependency(DRSFacts,object,P1,Arg,I2,['object->has_part']) :-
    get_has_part_from_object(DRSFacts,P1,Arg,I2).

get_dependency(DRSFacts,property,P1,Arg,I2,['ladj->rel']) :-
    get_rel_from_ladj(DRSFacts,P1,Arg,I2).

get_dependency(DRSFacts,property,P1,Arg,I2,['radj->rel']) :-
    get_rel_from_radj(DRSFacts,P1,Arg,I2).

get_dependency(DRSFacts,property,P1,Arg,I2,['rel->ladj']) :-
    get_ladj_from_rel(DRSFacts,P1,Arg,I2).

get_dependency(DRSFacts,property,P1,Arg,I2,['rel->radj']) :-
    get_radj_from_rel(DRSFacts,P1,Arg,I2).
        
/*
Helper functions
*/    
get_subject_from_verb(DRSFacts,VerbPredicate,Arg3,SubjectIndex) :- 
    get_arg3_from_verb_predicate(VerbPredicate,Arg3),
    member(object(Arg3,_,_,_,_,_)-SubjectIndex,DRSFacts).

get_object_from_verb(DRSFacts,VerbPredicate,Arg4,ObjectIndex) :- 
    get_arg4_from_verb_predicate(VerbPredicate,Arg4),
    member(object(Arg4,_,_,_,_,_)-ObjectIndex,DRSFacts).

get_iobject_from_verb(DRSFacts,VerbPredicate,Arg5,IObjectIndex) :- 
    get_arg5_from_verb_predicate(VerbPredicate,Arg5),
    member(object(Arg5,_,_,_,_,_)-IObjectIndex,DRSFacts).

get_pp_from_verb(DRSFacts,VerbPredicate,Arg1,PPIndex) :- 
    get_arg1_from_verb_predicate(VerbPredicate,Arg1),
    member(modifier_pp(Arg1,_,_)-PPIndex,DRSFacts).
    
get_adv_from_verb(DRSFacts,VerbPredicate,Arg1,AdvIndex) :- 
    get_arg1_from_verb_predicate(VerbPredicate,Arg1),
    member(modifier_adv(Arg1,_,_)-AdvIndex,DRSFacts).
    
get_dep_from_pp(DRSFacts,modifier_pp(_,_,V2)-_/_,V2,DepIndex) :-
    member(object(V2,_,_,_,_,_)-DepIndex,DRSFacts).

get_verb_from_pp(DRSFacts,modifier_pp(V1,_,_)-_/_,V1,VerbIndex) :-
    member(VerbPredicate,DRSFacts),
    get_arg1_from_verb_predicate(VerbPredicate,Arg1),
    V1 == Arg1,
    VerbPredicate = _-VerbIndex.
    
get_verb_from_adv(DRSFacts,modifier_adv(V1,_,_)-_/_,V1,VerbIndex) :- 
    member(VerbPredicate,DRSFacts),
    get_arg1_from_verb_predicate(VerbPredicate,Arg1),
    V1 == Arg1,
    VerbPredicate = _-VerbIndex.
		
get_adj_from_verb(DRSFacts,VerbPredicate,Arg4,AdjIndex) :-
    get_arg4_from_verb_predicate(VerbPredicate,Arg4),
    member(PropertyPredicate,DRSFacts),
    get_arg1_from_property_predicate(PropertyPredicate,Arg1),
    Arg4 == Arg1,
    PropertyPredicate = _-AdjIndex.
    
get_pobject_from_adj(DRSFacts,PropertyPredicate,Arg1,NounIndex) :-
    get_arg1_from_property_predicate(PropertyPredicate,Arg1),
    member(object(Arg1,_,_,_,_,_)-NounIndex,DRSFacts).
          
get_sobject_from_adj(DRSFacts,PropertyPredicate,Arg2,NounIndex) :-
    get_arg2_from_property_predicate(PropertyPredicate,Arg2),
    member(object(Arg2,_,_,_,_,_)-NounIndex,DRSFacts).
     
get_tobject_from_adj(DRSFacts,PropertyPredicate,Arg3,NounIndex) :-
    get_arg3_from_property_predicate(PropertyPredicate,Arg3),
    member(object(Arg3,_,_,_,_,_)-NounIndex,DRSFacts).
    
get_verb_from_adj(DRSFacts,PropertyPredicate,Arg1,VerbIndex) :-
    get_arg1_from_property_predicate(PropertyPredicate,Arg1),
    member(VerbPredicate,DRSFacts),
    get_arg4_from_verb_predicate(VerbPredicate,Arg4),
    Arg1 == Arg4,
    VerbPredicate = _-VerbIndex.
        
get_lobject_from_rel(DRSFacts,Predicate,V1,LObjectIndex) :-
    Predicate = relation(V1,of,_)-_/_,
    member(object(V1,_,_,_,_,_)-LObjectIndex,DRSFacts).

get_robject_from_rel(DRSFacts,Predicate,V2,RObjectIndex) :-
    Predicate = relation(_,of,V2)-_/_,
    member(object(V2,_,_,_,_,_)-RObjectIndex,DRSFacts).
    
get_verb_from_subject(DRSFacts,object(V1,_,_,_,_,_)-_/_,Arg3,VerbIndex) :-
    member(VerbPredicate,DRSFacts),
    get_arg3_from_verb_predicate(VerbPredicate,Arg3),
    Arg3 == V1,
    VerbPredicate = _-VerbIndex.
        
get_verb_from_object(DRSFacts,object(V1,_,_,_,_,_)-_/_,Arg4,VerbIndex) :-
    member(VerbPredicate,DRSFacts),
    get_arg4_from_verb_predicate(VerbPredicate,Arg4),
    Arg4 == V1,
    VerbPredicate = _-VerbIndex.

get_verb_from_iobject(DRSFacts,object(V1,_,_,_,_,_)-_/_,Arg5,VerbIndex) :-
    member(VerbPredicate,DRSFacts),
    get_arg5_from_verb_predicate(VerbPredicate,Arg5),
    Arg5 == V1,
    VerbPredicate = _-VerbIndex.

get_pp_from_dep(DRSFacts,object(V1,_,_,_,_,_)-_/_,V1,PPIndex) :-
    member(modifier_pp(_,_,V1)-PPIndex,DRSFacts).

get_pp_from_pp(DRSFacts,modifier_pp(V1,_,_)-PPIndex1,V1,PPIndex2) :-
    member(modifier_pp(V1,_,_)-PPIndex2,DRSFacts),
    PPIndex1 \= PPIndex2.

get_rel_from_pp(DRSFacts,modifier_pp(_,_,V1)-_/_,V1,RelIndex) :-
    member(relation(V1,of,_)-RelIndex,DRSFacts).

get_pp_from_rel(DRSFacts,relation(V1,of,_)-_/_,V1,PPIndex) :-
    member(modifier_pp(_,_,V1)-PPIndex,DRSFacts).
    
get_rel_from_lobject(DRSFacts,object(V1,_,_,_,_,_)-_/_,V1,LObjectIndex) :-
    member(relation(V1,of,_)-LObjectIndex,DRSFacts).

get_rel_from_robject(DRSFacts,object(V1,_,_,_,_,_)-_/_,V1,RObjectIndex) :-
    member(relation(_,of,V1)-RObjectIndex,DRSFacts).

get_rel_from_verb(DRSFacts,VerbPredicate,Arg4,RelIndex) :-
    get_arg4_from_verb_predicate(VerbPredicate,Arg4),
    member(relation(Arg4,of,_)-RelIndex,DRSFacts).

get_adj_from_pobject(DRSFacts,object(V1,_,_,_,_,_)-_/_,Arg1,AdjIndex) :-
    member(PropertyPredicate,DRSFacts),
    get_arg1_from_property_predicate(PropertyPredicate,Arg1),
    Arg1 == V1,
    PropertyPredicate = _-AdjIndex.
    
get_adj_from_sobject(DRSFacts,object(V1,_,_,_,_,_)-_/_,Arg2,AdjIndex) :-
    member(PropertyPredicate,DRSFacts),
    get_arg2_from_property_predicate(PropertyPredicate,Arg2),
    Arg2 == V1,
    PropertyPredicate = _-AdjIndex.
    
get_adj_from_tobject(DRSFacts,object(V1,_,_,_,_,_)-_/_,V1,AdjIndex) :-
    member(property(_,_,_,_,_,V1)-AdjIndex,DRSFacts).

get_object_from_has_part(DRSFacts,object(V1,na,_,_,_,_)-_/_,V2,ObjectIndex) :-
    member(has_part(V1,V2)-_,DRSFacts),
    member(object(V2,_,_,_,_,_)-ObjectIndex,DRSFacts).

get_has_part_from_object(DRSFacts,object(V2,na,_,_,_,_)-_/_,V1,ObjectIndex) :-
    member(has_part(V1,V2)-_,DRSFacts),
    member(object(V1,_,_,_,_,_)-ObjectIndex,DRSFacts).

get_rel_from_ladj(DRSFacts,PropertyPredicate,Arg1,RelIndex) :-
    get_arg1_from_property_predicate(PropertyPredicate,Arg1),
    member(relation(Arg1,_,_)-RelIndex,DRSFacts).

get_rel_from_radj(DRSFacts,PropertyPredicate,Arg1,RelIndex) :-
    get_arg1_from_property_predicate(PropertyPredicate,Arg1),
    member(relation(_,_,Arg1)-RelIndex,DRSFacts).

get_ladj_from_rel(DRSFacts,relation(Arg1,_,_)-_,Arg1,PropertyIndex) :-
    member(PropertyPredicate,DRSFacts),
    get_arg1_from_property_predicate(PropertyPredicate,Arg1),
    PropertyPredicate = _-PropertyIndex.

get_radj_from_rel(DRSFacts,relation(_,_,Arg3)-_,Arg3,PropertyIndex) :-
    member(PropertyPredicate,DRSFacts),
    get_arg1_from_property_predicate(PropertyPredicate,Arg3),
    PropertyPredicate = _-PropertyIndex.

get_self(_,_-Index,_,Index).

get_arg1_from_verb_predicate(predicate(V1,_,_)-_/_,V1).
get_arg1_from_verb_predicate(predicate(V1,_,_,_)-_/_,V1).
get_arg1_from_verb_predicate(predicate(V1,_,_,_,_)-_/_,V1).
get_arg3_from_verb_predicate(predicate(_,_,V2)-_/_,V2).
get_arg3_from_verb_predicate(predicate(_,_,V2,_)-_/_,V2).
get_arg3_from_verb_predicate(predicate(_,_,V2,_,_)-_/_,V2).
get_arg4_from_verb_predicate(predicate(_,_,_,V3)-_/_,V3).
get_arg4_from_verb_predicate(predicate(_,_,_,V3,_)-_/_,V3).
get_arg5_from_verb_predicate(predicate(_,_,_,_,V4)-_/_,V4).
get_arg1_from_property_predicate(property(V1,_,_)-_/_,V1).
get_arg1_from_property_predicate(property(V1,_,_,_)-_/_,V1).
get_arg1_from_property_predicate(property(V1,_,_,_,_,_)-_/_,V1).
get_arg2_from_property_predicate(property(_,_,_,V3)-_/_,V3).    
get_arg2_from_property_predicate(property(_,_,V2,_,_,_)-_/_,V2).
get_arg3_from_property_predicate(property(_,_,_,_,_,V5)-_/_,V5).