/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1  + verb(target) + noun2
Dependency graph: noun1(arg1) <--subject-- verb --object-->noun2(arg2)
Example: Mary owns a car.
*/
extract_semantic_role(v,Target,Conditions,Subject,Object) :- 
    get_verb_target(Conditions,Target,VerbPredicate),
    get_subject_from_verb(Conditions,VerbPredicate,Subject),
    get_object_from_verb(Conditions,VerbPredicate,Object).

/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1 + light verb + noun2(target) + of + noun3 
Dependency graph: noun1(arg1) <--subject-- light verb + noun2 <--nmod-- noun3(arg2)
Example: Mary has a job of a nurse.
*/
extract_semantic_role('lv+n',LightVerb-Target,Conditions,Subject,Object) :- 
    get_light_verb_target(Conditions,LightVerb-Target,LightVerbPredicate,TargetPredicate),
    get_noun_rel_mod(Conditions,TargetPredicate,Object),
    get_subject_from_verb(Conditions,LightVerbPredicate,Subject).

/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1 + light verb + adj + noun2(target)
Dependency graph: noun1(arg1) <--subject-- light verb --object--> noun2 <--amod-- adj(arg2)
Example: Mary has a nurse job. 
*/
extract_semantic_role('lv+n',LightVerb-Target,Conditions,Subject,Object) :- 
    get_light_verb_target(Conditions,LightVerb-Target,LightVerbPredicate,TargetPredicate),
    get_noun_amod(Conditions,TargetPredicate,Object),
    get_subject_from_verb(Conditions,LightVerbPredicate,Subject).
								
/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1's + noun2(target) + be + noun3 
Dependency graph: noun1(arg1) --nmod--> noun2 <--subject-- be --object--> noun3(arg2)
Example: Mary's age is 20. 
*/
extract_semantic_role('n+be',Target,Conditions,Subject,Object) :- 
    get_verb_target(Conditions,be,CopulaPredicate),
    get_target_from_copula(subject,Conditions,CopulaPredicate,Target,TargetPredicate),
    get_object_from_verb(Conditions,CopulaPredicate,Object),
    get_noun_rel_mod(Conditions,TargetPredicate,Subject).
								
/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1's + noun2(target) + be + adj
Dependency graph: noun1(arg1) --nmod--> noun2 <--subject-- be --amod--> adj(arg2) 
Example: Mary's age is young.
*/
extract_semantic_role('n+be',Target,Conditions,Subject,Object) :- 
    get_verb_target(Conditions,be,CopulaPredicate),
    get_target_from_copula(subject,Conditions,CopulaPredicate,Target,TargetPredicate),
    get_amod_from_verb(Conditions,CopulaPredicate,Object),
    get_noun_rel_mod(Conditions,TargetPredicate,Subject).
                                                                
/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1 + be + noun2's noun3(target)
Dependency graph: noun1(arg2) <--subject-- be --object--> noun3 <--nmod-- noun2(arg1)
Example: 20 is Mary's age.
*/
extract_semantic_role('be+n',Target,Conditions,Subject,Object) :- 
    get_verb_target(Conditions,be,CopulaPredicate),
    get_target_from_copula(object,Conditions,CopulaPredicate,Target,TargetPredicate),
    get_noun_rel_mod(Conditions,TargetPredicate,Object),
    get_subject_from_verb(Conditions,CopulaPredicate,Subject).
								
/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: 1) noun1 + be + adj(target)
		     2) noun1 + verb + adj(target) + noun2
Dependency graph: 1) noun1(arg1) <--subject-- be <--amod-- adj(arg2)
		  2) noun1 <--subject-- verb --object--> noun2(arg1) <--amod-- adj(arg2)
Example: 1) Mary is young.
	 2) Mary has a young daughter.
*/
extract_semantic_role(a,Target,Conditions,Subject,Object) :- 
    get_adj_target(Conditions,Target,PropPredicate),
    get_adj_target_subject(Conditions,PropPredicate,Subject),
    Object = Target.
								
/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: 1) noun1's + noun2(target) + of + noun3
		     2) noun1 + of + noun2(target) + of + noun3
Dependency graph: 1) noun1(arg1) --nmod--> noun2 <--nmod-- noun3(arg2)
		  2) noun1(arg1) <--nmod-- noun2 <--nmod-- noun3(arg2)
Example: 1) Mary's abandonment of a project
	 2) A man of an age of 20
*/
extract_semantic_role('of+n+of',Target,Conditions,Subject,Object) :- 
    get_target_noun(Conditions,Target,TargetPredicate),
    get_fe_from_target_noun(Conditions,TargetPredicate,Subject,Object).

/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1 + of + adj + noun2(target)
Dependency graph: noun1(arg1) <--nmod-- noun2 <--amod-- adj(arg2)
Example: A lady of an italian origin
*/
extract_semantic_role('of+n+of',Target,Conditions,Subject,Object) :- 
    get_target_noun(Conditions,Target,object(W1,Target,W2,W3,W4,W5)-W6/W7),
    basics:member(relation(X1,of,X2)-X3/X4,Conditions),
    X2 == W1,
    get_noun_from_of_relation(Conditions,relation(X1,of,X2)-X3/X4,0,Subject),
    get_noun_amod(Conditions,object(W1,Target,W2,W3,W4,W5)-W6/W7,Object).
								
/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1 + noun2 + be + noun3(target)
Dependency graph: noun1(arg1) & noun2(arg2) <--subject-- be --object--> noun3
Example: Mary and Sarah are friends.
*/
extract_semantic_role('has_part+be+n',Target,Conditions,Subject,Object) :- 
    get_verb_target(Conditions,be,CopulaPredicate), 
    get_target_from_copula(object,Conditions,CopulaPredicate,Target,TargetPredicate),
    get_has_part_from_verb(subject,Conditions,CopulaPredicate,Subject,Object).
								
/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1 + verb(target) + pp + noun2
Dependency graph: noun1(arg1) <--subject-- verb <--pmod-- pp --object--> noun2(arg2)
Example: Mary works as a nurse.
*/
extract_semantic_role('v+pp',Verb-PP,Conditions,Subject,Object) :- 
    get_verb_target(Conditions,Verb,VerbPredicate),
    get_pp_from_verb(Conditions,VerbPredicate,PP,PPPredicate),
    get_subject_from_verb(Conditions,VerbPredicate,Subject),
    get_noun_from_pp(Conditions,PPPredicate,Object).
								
/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1 + be + pp + noun2(target) + of + noun3
Dependency graph: noun1(arg1) <--subject-- be <--pmod-- pp --object--> noun2 <--nmod-- noun3(arg2)
Example: Mary is at an age of 20.
*/
extract_semantic_role('be+pp+n',PP-Noun,Conditions,Subject,Object) :- 
    get_verb_target(Conditions,be,CopulaPredicate),
    get_pp_from_verb(Conditions,CopulaPredicate,PP,PPPredicate),
    get_subject_from_verb(Conditions,CopulaPredicate,Subject),
    get_target_from_pp(Conditions,PPPredicate,Noun,TargetPredicate),
    get_noun_rel_mod(Conditions,TargetPredicate,Object).

/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1 + be + pp + adj + noun2(target)
Dependency graph: noun1(arg1) <--subject-- be <--pmod-- pp --object--> noun2 <--amod-- adj(arg2)
Example: Mary is at a young age.
*/
extract_semantic_role('be+pp+n',PP-Noun,Conditions,Subject,Object) :- 
    get_verb_target(Conditions,be,CopulaPredicate),                                                                  
    get_pp_from_verb(Conditions,CopulaPredicate,PP,PPPredicate),
    get_subject_from_verb(Conditions,CopulaPredicate,Subject),
    get_target_from_pp(Conditions,PPPredicate,Noun,TargetPredicate),
    get_noun_amod(Conditions,TargetPredicate,Object).
							      
/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1's + noun2(target)/noun2(target) of noun1
Dependency graph: noun1(arg1) --nmod--> noun2(arg2)
Example: Mary's father is a teacher 
*/
extract_semantic_role('n+of+n',Target,Conditions,Subject,Object) :- 
    get_target_noun(Conditions,Target,TargetPredicate),
    get_noun_rel_mod(Conditions,TargetPredicate,Subject),
    Object = Target.

/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1 + be + noun2(target)
Dependency graph: noun1 <--subject-- be --object--> noun2(arg2)
Example: Mary is a nurse.
*/
extract_semantic_role('n+be+n',Target,Conditions,Subject,Object) :- 
    get_verb_target(Conditions,be,CopulaPredicate),
    get_target_from_copula(object,Conditions,CopulaPredicate,Target,TargetPredicate),
    get_subject_from_verb(Conditions,CopulaPredicate,Subject),
    Object = Target.

/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1 + of(target) + noun2
Dependency graph: noun1(arg1) <--nmod-- noun2(arg2)
Example: John is a man of 13.
*/
extract_semantic_role(of,Target,Conditions,Subject,Object) :- 
    basics:member(relation(X1,of,X2)-X3/X4,Conditions),
    get_noun_from_of_relation(Conditions,relation(X1,of,X2)-X3/X4,0,Subject), 
    get_noun_from_of_relation(Conditions,relation(X1,of,X2)-X3/X4,1,Object). 

/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1 + be + adj(target) + pp + noun2
Dependency graph: noun1(arg1) <--subject-- be <--amod-- adj <--pmod-- pp <--nmod--  noun2(arg2)
Example: John is married to Marry.
*/
extract_semantic_role('be+a+pp',AP,Conditions,Subject,Object) :- 
    AP = Target-PP,
    get_verb_target(Conditions,be,CopulaPredicate),
    get_amod_from_verb(Conditions,CopulaPredicate,Adj),   
    Adj == Target,								 
    get_pp_from_verb(Conditions,CopulaPredicate,PP,PPPredicate),
    get_subject_from_verb(Conditions,CopulaPredicate,Subject),
    get_noun_from_pp(Conditions,PPPredicate,Object).
								 
/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1 + noun2 + be + adj(target)
Dependency graph: noun1(arg1) & noun2(arg2) <--subject-- be <--amod-- adj
Example: John and Marry are married.
*/
extract_semantic_role('has_part+be+a',Target,Conditions,Subject,Object) :- 
    get_verb_target(Conditions,be,CopulaPredicate),
    get_amod_from_verb(Conditions,CopulaPredicate,Adj),
    Adj == Target, 
    get_has_part_from_verb(subject,Conditions,CopulaPredicate,Subject,Object).

/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1 + noun2 +verb(target) 
Dependency graph: noun1(arg1) & noun2(arg2) <--subject-- verb 
Example: John and Marry marry in a church.
*/
extract_semantic_role('has_part+v',Target,Conditions,Subject,Object) :-
    get_verb_target(Conditions,Target,TargetPredicate),
    get_has_part_from_verb(subject,Conditions,TargetPredicate,Subject,Object).

/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1 + noun2 + be + pp + noun3(target)
Dependency graph: noun1(arg1) & noun2(arg2) <--subject-- be <--pmod-- pp <--nmod-- noun3
Example: John and Marry are in a marriage.
*/
extract_semantic_role('has_part+be+pp+n',PPN,Conditions,Subject,Object) :- 
    PPN = PP-Target,
    get_verb_target(Conditions,be,CopulaPredicate),
    get_pp_from_verb(Conditions,CopulaPredicate,PP,PPPredicate),
    get_noun_from_pp(Conditions,PPPredicate,Noun),
    Noun == Target,
    get_has_part_from_verb(subject,Conditions,CopulaPredicate,Subject,Object).

/*
Usage: extract_semantic_role(+Pattern,+Target,+Conditions,-Subject,-Object)
Grammatical Pattern: noun1 + be + (adj + pp)(target) + noun2
Dependency graph: noun1(arg1) <--subject-- be <--amod-- adj <--pmod-- pmod  <--nmod-- noun2(arg2)
Example: Marry is afraid-of a spider.
*/
extract_semantic_role('be+ap',AdjPP,Conditions,Subject,Object) :-
    get_verb_target(Conditions,be,CopulaPredicate),
    get_amod_pp_predicate_from_verb(Conditions,CopulaPredicate,AdjPP,AdjPPPredicate),
    get_noun_from_amod_pp(Conditions,AdjPPPredicate,Object),
    get_subject_from_verb(Conditions,CopulaPredicate,Subject).
									   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
Helper functions
*/
get_light_verb_target(Conditions,VP,LightVerbPredicate,TargetPredicate) :- 
    VP = LightVerb-Target,
    basics:member(predicate(V1,LightVerb,V2,V3)-V4/V5,Conditions),
    basics:member(object(W1,Target,W2,W3,W4,W5)-W6/W7,Conditions),
    V3 == W1,
    LightVerbPredicate = predicate(V1,LightVerb,V2,V3)-V4/V5,
    TargetPredicate = object(W1,Target,W2,W3,W4,W5)-W6/W7. 


get_noun_lexicon_from_var_helper(Arg,Conditions,Noun) :- 
    var(Arg),!,
    basics:member(object(V1,V,V2,V3,V4,V5)-V6/V7,Conditions),
    Arg == V1,
    Noun = V.

get_noun_lexicon_from_var_helper(named(X),_,X).
get_noun_lexicon_from_var_helper(string(X),_,X).
get_noun_lexicon_from_var_helper(int(X),_,X).

get_measuring_noun_lexicon_from_var(Arg,Conditions,Noun) :- 
    var(Arg),
    basics:member(object(V1,V,V2,V3,V4,V5)-V6/V7,Conditions),
    Arg == V1,
    measuring_noun(V),
    basics:member(relation(W1,of,W2)-W3/W4,Conditions),
    W1 == V1,
    get_noun_lexicon_from_var_helper(W2,Conditions,Noun).

get_noun_lexicon_from_var(Arg,Conditions,Noun) :- 
    get_measuring_noun_lexicon_from_var(Arg,Conditions,Noun),!.

get_noun_lexicon_from_var(Arg,Conditions,Noun) :- 
    get_noun_lexicon_from_var_helper(Arg,Conditions,Noun).     

get_noun_predicate_from_var(Arg,Conditions,NounPredicate) :- 
    var(Arg),
    basics:member(object(V1,V,V2,V3,V4,V5)-V6/V7,Conditions),
    Arg == V1,
    NounPredicate = object(V1,V,V2,V3,V4,V5)-V6/V7.

get_noun_rel_mod(Conditions,Predicate,TargetModifier) :- 
    Predicate = object(W1,Target,W2,W3,W4,W5)-W6/W7,
    basics:member(relation(A,of,B)-_/_,Conditions),
    A == W1,
    get_noun_lexicon_from_var(B,Conditions,TargetModifier).

get_noun_amod(Conditions,Predicate,TargetModifier) :- 
    Predicate = object(W1,Target,W2,W3,W4,W5)-W6/W7,
    basics:member(property(A,B,_)-_/_,Conditions),
    A == W1,
    TargetModifier = B.

get_target_from_copula(subject,Conditions,Predicate,Target,TargetPredicate) :- 
    Predicate = predicate(V1,be,V2,V3)-V4/V5,
    basics:member(object(W1,Target,W2,W3,W4,W5)-W6/W7,Conditions),  
    V2 == W1,
    TargetPredicate = object(W1,Target,W2,W3,W4,W5)-W6/W7.

get_target_from_copula(object,Conditions,Predicate,Target,TargetPredicate) :- 
    Predicate = predicate(V1,be,V2,V3)-V4/V5,
    basics:member(object(W1,Target,W2,W3,W4,W5)-W6/W7,Conditions),  
    V3 == W1,
    TargetPredicate = object(W1,Target,W2,W3,W4,W5)-W6/W7.

get_verb_target(Conditions,Target,VerbPredicate) :- 
    basics:member(predicate(V1,V,V2,V3)-V4/V5,Conditions), 
    V == Target,!,
    VerbPredicate = predicate(V1,V,V2,V3)-V4/V5.

get_verb_target(Conditions,Target,VerbPredicate) :- 
    basics:member(predicate(V1,V,V2)-V3/V4,Conditions),
    V == Target,!,
    VerbPredicate = predicate(V1,V,V2)-V3/V4.

get_pp_from_verb(Conditions,predicate(W1,W,W2)-W3/W4,Target,PPPredicate) :- 
    basics:member(modifier_pp(V1,V,V2)-V3/V4,Conditions),
    V == Target,
    V1 == W1,
    PPPredicate = modifier_pp(V1,V,V2)-V3/V4.
	
get_pp_from_verb(Conditions,predicate(W1,W,W2,W3)-W4/W5,Target,PPPredicate) :- 
    basics:member(modifier_pp(V1,V,V2)-V3/V4,Conditions),
    V == Target,
    V1 == W1,
    PPPredicate = modifier_pp(V1,V,V2)-V3/V4.

get_subject_from_verb(Conditions,Predicate,Subject) :- 
    Predicate = predicate(V1,V,V2,V3)-V4/V5,
    get_noun_lexicon_from_var(V2,Conditions,Subject).

get_subject_from_verb(Conditions,Predicate,Subject) :- 
    Predicate = predicate(V1,V,V2)-V3/V4,					       
    get_noun_lexicon_from_var(V2,Conditions,Subject).

get_object_from_verb(Conditions,predicate(V1,V,V2,V3)-V4/V5,Object) :- 
    get_noun_lexicon_from_var(V3,Conditions,Object).

get_amod_from_verb(Conditions,predicate(V1,V,V2,V3)-V4/V5,Object) :- 
    basics:member(property(W1,W,W2)-W3/W4,Conditions),
    W1 == V3,
    Object = W.

get_amod_pp_predicate_from_verb(Conditions,predicate(V1,V,V2,V3)-V4/V5,
    Target,TargetPredicate) :-
    basics:member(property(W1,W,W2,W3)-W4/W5,Conditions),
    W1 == V3,
    Target = X1-X2,
    string:concat_atom([X1,-,X2],TempStr),
    W == TempStr,
    TargetPredicate = property(W1,W,W2,W3)-W4/W5.

get_adj_target(Conditions,Target,PropPredicate) :- 
    basics:member(property(W1,W,W2)-W3/W4,Conditions),
    W == Target,
    PropPredicate = property(W1,W,W2)-W3/W4.

get_adj_target_subject(Conditions,property(V1,V,V2)-V3/V4,Subject) :- 
    basics:member(predicate(W1,be,W2,W3)-W4/W5,Conditions),
    W3 == V1,!,
    get_subject_from_verb(Conditions,predicate(W1,be,W2,W3)-W4/W5,Subject).

get_adj_target_subject(Conditions,property(V1,V,V2)-V3/V4,Subject) :- 
    basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
    V1 == W1,
    Subject = W.

get_target_noun(Conditions,Target,TargetPredicate) :- 
    basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
    W == Target,
    TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7.

get_fe_from_target_noun(Conditions,object(V1,V,V2,V3,V4,V5)-V6/V7,Subject,Object) :- 
    basics:member(relation(X1,of,X2)-X3/X4,Conditions),
    X1 == V1,
    basics:member(relation(Y1,of,Y2)-Y3/Y4,Conditions),
    Y1 == V1,
    X4 < Y4,
    get_noun_from_of_relation(Conditions,relation(X1,of,X2)-X3/X4,1,Subject),
    get_noun_from_of_relation(Conditions,relation(Y1,of,Y2)-Y3/Y4,1,Object).

get_fe_from_target_noun(Conditions,object(V1,V,V2,V3,V4,V5)-V6/V7,Subject,Object) :- 
    basics:member(relation(X1,of,X2)-X3/X4,Conditions),
    X2 == V1,
    basics:member(relation(Y1,of,Y2)-Y3/Y4,Conditions),
    Y1 == V1,
    get_noun_from_of_relation(Conditions,relation(X1,of,X2)-X3/X4,0,Subject),
    get_noun_from_of_relation(Conditions,relation(Y1,of,Y2)-Y3/Y4,1,Object).
							 

get_noun_from_of_relation(Conditions,Predicate,0,Noun) :- 
    Predicate = relation(X1,of,X2)-X3/X4,
    get_noun_lexicon_from_var(X1,Conditions,Noun).

get_noun_from_of_relation(Conditions,Predicate,1,Noun) :- 
    Predicate = relation(X1,of,X2)-X3/X4,
    get_noun_lexicon_from_var(X2,Conditions,Noun).

get_noun_from_has_part(Conditions,Predicate,Noun) :- 
    Predicate = has_part(X1,X2)-X3/X4,
    get_noun_lexicon_from_var(X2,Conditions,Noun).

get_noun_from_pp(Conditions,modifier_pp(X1,X,X2)-X3/X4,Noun) :- 
    get_noun_lexicon_from_var(X2,Conditions,Noun).

get_noun_from_amod_pp(Conditions,property(W1,W,W2,W3)-W4/W5,Noun) :-
    get_noun_lexicon_from_var(W3,Conditions,Noun).

get_target_from_pp(Conditions,modifier_pp(X1,X,X2)-X3/X4,Target,TargetPredicate) :- 
    basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
    W1 == X2,
    Target == W,
    TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7.

get_has_part_from_verb(subject,Conditions,Predicate,Subject,Object) :- 
    Predicate = predicate(W1,W,W2,W3)-W4/W5,
    basics:member(has_part(X1,X2)-X3/X4,Conditions),
    X1 == W2,
    basics:member(has_part(Y1,Y2)-Y3/Y4,Conditions),
    Y1 == W2,
    X2 \= Y2,
    get_noun_from_has_part(Conditions,has_part(X1,X2)-X3/X4,Subject),
    get_noun_from_has_part(Conditions,has_part(Y1,Y2)-Y3/Y4,Object).

get_has_part_from_verb(subject,Conditions,Predicate,Subject,Object) :- 
    Predicate = predicate(W1,W,W2)-W3/W4,
    basics:member(has_part(X1,X2)-X3/X4,Conditions),
    X1 == W2,
    basics:member(has_part(Y1,Y2)-Y3/Y4,Conditions),
    Y1 == W2,
    X2 \= Y2,
    get_noun_from_has_part(Conditions,has_part(X1,X2)-X3/X4,Subject),
    get_noun_from_has_part(Conditions,has_part(Y1,Y2)-Y3/Y4,Object).
									 
get_has_part_from_verb(object,Conditions,Predicate,Subject,Object) :- 
    Predicate = predicate(W1,W,W2,W3)-W4/W5,
    basics:member(has_part(X1,X2)-X3/X4,Conditions),
    X1 == W3,
    basics:member(has_part(Y1,Y2)-Y3/Y4,Conditions),
    Y1 == W3,
    X2 \= Y2,
    get_noun_from_has_part(Conditions,has_part(X1,X2)-X3/X4,Subject),
    get_noun_from_has_part(Conditions,has_part(Y1,Y2)-Y3/Y4,Object).
