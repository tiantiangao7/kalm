get_frame_element(Conditions,Target,Pattern,Frame) :- frame(Target,Pattern,FName,RoleList),
						 extract_semantic_role(Pattern,Target,Conditions,Subject,Object),
						 cat(Subject,SubCat),
						 basics:member(role(X1,subject,SubCat),RoleList),
						 cat(Object,ObjCat),
						 basics:member(role(X2,object,ObjCat),RoleList),
						 Frame =.. [FName,[role(X1,Subject),role(X2,Object)]].

/*
extract_semantic_role(Conditions,Target,POS,Subject,Object) :- (POS = v
								->
								get_verb(Conditions,Target,VerbPredicate),
								get_subject_from_verb(Conditions,VerbPredicate,Subject),
							        get_object_from_verb(Conditions,VerbPredicate,Object)
								;
								POS = 'lv+n', contain_light_verb(Conditions,Target,LightVerb,TargetPredicate),
								get_target_rel_mod(Conditions,TargetPredicate,Object)
								->
								get_subject_from_verb(Conditions,LightVerb,Subject)
								;	
								POS = 'lv+n', contain_light_verb(Conditions,Target,LightVerb,TargetPredicate),
                                                                get_target_prop_mod(Conditions,TargetPredicate,Object)
                                                                ->
                                                                get_subject_from_verb(Conditions,LightVerb,Subject)
								;
								POS = 'n+be', contain_copula(Conditions,CopulaPredicate),
								get_target_from_copula(Conditions,CopulaPredicate,Target,
										       TargetPredicate,subject),
								get_object_from_verb(Conditions,CopulaPredicate,Object)
								->
								get_target_rel_mod(Conditions,TargetPredicate,Subject)
								;
								POS = 'n+be', contain_copula(Conditions,CopulaPredicate),
                                                                get_target_from_copula(Conditions,CopulaPredicate,Target,
                                                                                       TargetPredicate,subject),
								get_prop_from_verb(Conditions,CopulaPredicate,Object)
                                                                ->
                                                                get_target_rel_mod(Conditions,TargetPredicate,Subject)
                                                                ;
								POS = 'be+n', contain_copula(Conditions,CopulaPredicate),
                                                                get_target_from_copula(Conditions,CopulaPredicate,Target,
                                                                                       TargetPredicate,object)
                                                                ->
                                                                get_target_rel_mod(Conditions,TargetPredicate,Object),
                                                                get_subject_from_verb(Conditions,CopulaPredicate,Subject)
								;
								POS = a,get_prop_target(Conditions,Target,PropPredicate)
								->
								get_prop_target_subject(Conditions,PropPredicate,Subject),
								Object = Target
								;
								POS = 'of+n+of',get_target_noun(Conditions,Target,TargetPredicate)
								->
								get_fe_from_target_noun(Conditions,TargetPredicate,Subject,Object)
								;
								POS = 'has_part+be+n',contain_copula(Conditions,CopulaPredicate),
								get_target_from_copula(Conditions,CopulaPredicate,friend,
										       TargetPredicate,object)
								->
								get_has_part_from_verb(Conditions,CopulaPredicate,subject,
										       Subject,Object)
								;
								POS = 'v+pp',Target = Verb-PP
								->
								get_verb(Conditions,Verb,VerbPredicate),
								get_pp_from_verb(Conditions,VerbPredicate,PP,PPPredicate),
								get_subject_from_verb(Conditions,VerbPredicate,Subject),
								get_noun_from_pp(Conditions,PPPredicate,Object)
								;
								POS = 'be+pp+n',Target = PP-Noun
							        ->
								contain_copula(Conditions,CopulaPredicate),
								get_pp_from_verb(Conditions,CopulaPredicate,PP,PPPredicate),
								get_subject_from_verb(Conditions,CopulaPredicate,Subject),
								get_target_from_pp(Conditions,PPPredicate,Noun,TargetPredicate),
								get_target_rel_mod(Conditions,TargetPredicate,Object)
							       ).

contain_light_verb(Conditions,Target,
                   LightVerb,TargetPredicate) :- basics:member(predicate(V1,Verb,V2,V3)-V4/V5,Conditions),
					         basics:member(object(W1,Target,W2,W3,W4,W5)-W6/W7,Conditions),
				  		 light_verb(Verb),
						 V3 == W1,
						 LightVerb = predicate(V1,Verb,V2,V3)-V4/V5,
						 TargetPredicate = object(W1,Target,W2,W3,W4,W5)-W6/W7. 

get_target_rel_mod(Conditions,
	           object(W1,Target,W2,W3,W4,W5)-W6/W7,TargetModifier) :- basics:member(relation(A,of,B)-_/_,Conditions),
		       						          A == W1,
								          (var(B)
									   ->
									   basics:member(object(V1,V,V2,V3,V4,V5)-V6/V7,Conditions),
								           B == V1,
								           TargetModifier = V
									   ;
									   B = named(X)
                                                                           ->
                                                                           TargetModifier = X
                                                                           ;
                                                                           B = int(X)
                                                                           ->
                                                                           TargetModifier = X
								          ).

get_target_prop_mod(Conditions,
                    object(W1,Target,W2,W3,W4,W5)-W6/W7,TargetModifier) :- basics:member(property(A,B,_)-_/_,Conditions),
                                                                           A == W1,
                                                                           TargetModifier = B.

contain_copula(Conditions,CopulaPredicate) :- X = be, 
					      basics:member(predicate(V1,X,V2,V3)-V4/V5,Conditions),
					      CopulaPredicate = predicate(V1,X,V2,V3)-V4/V5. 

contain_copula(Conditions,CopulaPredicate) :- X = be, 
                                              basics:member(predicate(V1,X,V2)-V3/V4,Conditions),
                                              CopulaPredicate = predicate(V1,X,V2)-V3/V4.

get_target_from_copula(Conditions,predicate(V1,be,V2,V3)-V4/V5,
		       Target,TargetPredicate,Role) :- basics:member(object(W1,Target,W2,W3,W4,W5)-W6/W7,Conditions),  
                            			       (V2 == W1
							->
							Role = subject
							;
							V3 == W1
							->
							Role = object
						       ),
						       TargetPredicate = object(W1,Target,W2,W3,W4,W5)-W6/W7.

get_verb(Conditions,Target,VerbPredicate) :- basics:member(predicate(V1,V,V2,V3)-V4/V5,Conditions), 
			        	     V == Target,!,
                                             VerbPredicate = predicate(V1,V,V2,V3)-V4/V5.

get_verb(Conditions,Target,VerbPredicate) :- basics:member(predicate(V1,V,V2)-V3/V4,Conditions),
                                             V == Target,!,
                                             VerbPredicate = predicate(V1,V,V2)-V3/V4.

get_pp_from_verb(Conditions,predicate(W1,W,W2)-W3/W4,
				Target, PPPredicate) :- basics:member(modifier_pp(V1,V,V2)-V3/V4,Conditions),
							V == Target,
							V1 == W1,
							PPPredicate = modifier_pp(V1,V,V2)-V3/V4.

get_subject_from_verb(Conditions,predicate(V1,V,V2,V3)-V4/V5,Subject) :- (var(V2)
			  						  ->
							        	  basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
									  W1 == V2,
							        	  Subject = W	
								          ;
									  V2 = named(W)
									  ->
									  Subject = W
									  ;
									  V2 = int(W)
									  -> 
									  Subject = W
									  ;
									  fail % or Subject = V2
							       		 ).

get_subject_from_verb(Conditions,predicate(V1,V,V2)-V3/V4,Subject) :- (var(V2)
                                                                       ->
                                                                       basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
                                                                       W1 == V2,
                                                                       Subject = W  
                                                                       ;
                                                                       V2 = named(W)
                                                                       ->
                                                                       Subject = W
                                                                       ;
                                                                       V2 = int(W)
                                                                       ->
                                                                       Subject = W
                                                                       ;
                                                                       fail % or Subject = V2
                                                                      ).

get_object_from_verb(Conditions,predicate(V1,V,V2,V3)-V4/V5,Object) :- (var(V3)
                                                                        ->
                                                                        basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
                                                                        W1 == V3,
                                                                        Object = W
                                                                        ;
                                                                        V3 = named(W)
                                                                        ->
                                                                        Object = W
                                                                        ;
                                                                        V3 = int(W)
                                                                        ->
                                                                        Object = W
									;
									fail
                                                                       ).

get_prop_from_verb(Conditions,predicate(V1,V,V2,V3)-V4/V5,Object) :- basics:member(property(W1,W,W2)-W3/W4,Conditions),
                                                                     W1 == V3,
                                                                     Object = W.

get_prop_target(Conditions,Target,PropPredicate) :- basics:member(property(W1,W,W2)-W3/W4,Conditions),
	     				            W == Target,
					            PropPredicate = property(W1,W,W2)-W3/W4.

get_prop_target_subject(Conditions,property(V1,V,V2)-V3/V4,Subject) :- basics:member(predicate(W1,be,W2,W3)-W4/W5,Conditions),
								       W3 == V1,!,
								       get_subject_from_verb(Conditions,predicate(W1,be,W2,W3)-W4/W5,Subject).

get_prop_target_subject(Conditions,property(V1,V,V2)-V3/V4,Subject) :- basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
                                                                       V1 == W1,
                                                                       Subject = W.

get_target_noun(Conditions,Target,TargetPredicate) :- basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
						      W == Target,
						      TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7.

get_fe_from_target_noun(Conditions,
			object(V1,V,V2,V3,V4,V5)-V6/V7,Subject,Object) :- basics:member(relation(X1,of,X2)-X3/X4,Conditions),
								      X1 == V1,
								      basics:member(relation(Y1,of,Y2)-Y3/Y4,Conditions),
								      Y1 == V1,
								      X4 < Y4,
								      get_noun_from_of_relation(Conditions,relation(X1,of,X2)-X3/X4,1,Subject),
								      get_noun_from_of_relation(Conditions,relation(Y1,of,Y2)-Y3/Y4,1,Object).

get_fe_from_target_noun(Conditions,
                        object(V1,V,V2,V3,V4,V5)-V6/V7,Subject,Object) :- basics:member(relation(X1,of,X2)-X3/X4,Conditions),
                                                                      X2 == V1,
                                                                      basics:member(relation(Y1,of,Y2)-Y3/Y4,Conditions),
                                                                      Y1 == V1,
                                                                      get_noun_from_of_relation(Conditions,relation(X1,of,X2)-X3/X4,0,Subject),
                                                                      get_noun_from_of_relation(Conditions,relation(Y1,of,Y2)-Y3/Y4,1,Object).
							 

get_noun_from_of_relation(Conditions,relation(X1,of,X2)-X3/X4,0,Noun) :- (var(X1)
                                                                          ->
                                                                          basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
                                                                          W1 == X1,
                                                                          Noun = W
                                                                          ;
                                                                          X1 = named(W)
                                                                          ->
                                                                          Noun = W
                                                                          ;
                                                                          X1 = int(W)
                                                                          ->
                                                                          Noun = W
                                                                          ;
                                                                          fail
                                                                         ).

get_noun_from_of_relation(Conditions,relation(X1,of,X2)-X3/X4,1,Noun) :- (var(X2)
                                                                          ->
                                                                          basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
                                                                          W1 == X2,
                                                                          Noun = W
                                                                          ;
                                                                          X2 = named(W)
                                                                          ->
                                                                          Noun = W
                                                                          ;
                                                                          X2 = int(W)
                                                                          ->
                                                                          Noun = W
                                                                          ;
                                                                          fail
                                                                         ). 

get_noun_from_has_part(Conditions,has_part(X1,X2)-X3/X4,Noun) :- (var(X2)
                                                                  ->
                                                                  basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
                                                                  W1 == X2,
                                                                  Noun = W
                                                                  ;
                                                                  X2 = named(W)
                                                                  ->
                                                                  Noun = W
                                                                  ;
                                                                  X2 = int(W)
                                                                  ->
                                                                  Noun = W
                                                                  ;
                                                                  fail
                                                                 ).

get_noun_from_pp(Conditions,modifier_pp(X1,X,X2)-X3/X4,Noun) :- (var(X2)
                                                                 ->
                                                                 basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
                                                                 W1 == X2,
                                                                 Noun = W
                                                                 ;
                                                                 X2 = named(W)
                                                                 ->
                                                                 Noun = W
                                                                 ;
                                                                 X2 = int(W)
                                                                 ->
                                                                 Noun = W
                                                                 ;
                                                                 fail
                                                                ).

get_target_from_pp(Conditions,modifier_pp(X1,X,X2)-X3/X4,
			      Target,TargetPredicate) :- basics:member(object(W1,W,W2,W3,W4,W5)-W6/W7,Conditions),
                                                         W1 == X2,
                                                         Target == W,
							 TargetPredicate = object(W1,W,W2,W3,W4,W5)-W6/W7.

get_has_part_from_verb(Conditions,predicate(W1,W,W2,W3)-W4/W5,
		      				Role,Subject,Object) :- (Role = subject
									 ->
									 basics:member(has_part(X1,X2)-X3/X4,Conditions),
									 X1 == W2,
									 basics:member(has_part(Y1,Y2)-Y3/Y4,Conditions),
									 Y1 == W2,
									 X2 \= Y2,
									 get_noun_from_has_part(Conditions,has_part(X1,X2)-X3/X4,Subject),
									 get_noun_from_has_part(Conditions,has_part(Y1,Y2)-Y3/Y4,Object)
									 ;
		      							 Role = object
									 ->
									 basics:member(has_part(X1,X2)-X3/X4,Conditions),
									 X1 == W3,
									 basics:member(has_part(Y1,Y2)-Y3/Y4,Conditions),
									 Y1 == W3,
									 X2 \= Y2,
									 get_noun_from_has_part(Conditions,has_part(X1,X2)-X3/X4,Subject),
									 get_noun_from_has_part(Conditions,has_part(Y1,Y2)-Y3/Y4,Object)
									 ;
									 fail
									).
*/

% world knowledge
measuring_noun(cup).
measuring_noun(box).
measuring_noun(bottle).

% frame knowledge
frame(own,v,possession,[role(owner,subject,person),role(possession,object,object)]).
frame(age,'n+be',age,[role(entity,subject,person),role(age,object,age)]).
frame(age,'be+n',age,[role(entity,object,person),role(age,subject,age)]).
frame(have-age,'lv+n',age,[role(entity,subject,person),role(age,object,age)]).
frame(age,'of+n+of',age,[role(entity,subject,person),role(age,object,age)]).
frame(at-age,'be+pp+n',age,[role(entity,subject,person),role(age,object,age)]).
frame(young,a,age,[role(entity,subject,person),role(age,object,age)]).
frame(friend,'has_part+be+n',friend,[role(entity1,subject,person),role(entity2,object,person)]).
frame(work-as,'v+pp','being_employed',[role(employee,subject,person),role(position,object,profession)]).
frame(work-for,'v+pp','being_employed',[role(employee,subject,person),role(employer,object,company)]).
frame(work-in,'v+pp','being_employed',[role(employee,subject,person),role(place,object,location)]).
frame(abandonment,'of+n+of',abandonment,[role(agent,subject,person),role(theme,object,abstract)]).
frame(father,'n+of+n',kinship,[role(ego,subject,person),role(alter,object,person)]).
frame(italian,'n+be+n','people_by_origin',[role(person,subject,person),role(origin,object,nationality)]).
frame(origin,'of+n+of','people_by_origin',[role(person,subject,person),role(origin,object,nationality)]).
frame(of,of,age,[role(entity,subject,person),role(age,object,age)]).
frame(married-to,'be+a+pp','personal_relationship',[role(entity1,subject,person),role(entity2,object,person)]).
frame(in-marriage,'has_part+be+pp+n','personal_relationship',[role(entity1,subject,person),role(entity2,object,person)]).
frame(married,'has_part+be+a','personal_relationship',[role(entity1,subject,person),role(entity2,object,person)]).
frame(afraid-of,'be+ap','experiencer_focused_emotion',[role(experiencer,subject,person),role(content,object,animal)]).

% babelnet knowledge base
cat('Mary',person).
cat(car,object).
cat(water,object).
cat(young,age).
cat(old,age).
cat(ancient,age).
cat(13,age).
cat(lady,person).
cat('John',person).
cat(office,location).
cat(nurse,profession).
cat('Amazon',company).
cat(project,abstract).
cat(father,person).
cat(italian,nationality).
cat(spider,animal).

% query
%acetext_to_drs('13 is Mary\'s age.',S,T,drs(Refs,Conditions),M),get_frame_element(Conditions,age,n,L).
%acetext_to_drs('Mary\'s age is 13.',S,T,drs(Refs,Conditions),M),get_frame_element(Conditions,age,n,L).
%acetext_to_drs('Mary has an age of 13.',S,T,drs(Refs,Conditions),M),get_frame_element(Conditions,age,n,L).
%acetext_to_drs('Mary owns a car.',S,T,drs(Refs,Conditions),M),get_frame_element(Conditions,own,v,L).
%acetext_to_drs('Mary is an owner of a car.',S,T,drs(Refs,Conditions),M),get_frame_element(Conditions,owner,n,L).
%acetext_to_drs('A car has an owner of Mary.',S,T,drs(Refs,Conditions),M),get_frame_element(Conditions,owner,n,L).
