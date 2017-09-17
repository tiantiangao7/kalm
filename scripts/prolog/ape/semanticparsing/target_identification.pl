extract_target_predicate(Conditions,List) :- extract_target_predicate_helper2(Conditions,PredicateList),
					     extract_target_predicate_helper1(Conditions,PredicateList,List).

extract_target_predicate_helper1(_,[],[]).
extract_target_predicate_helper1(Conditions,
				 [Predicate|RestPredicateList],List) :- 
							(extract_property_target(Conditions,Predicate,L2),L2 \= []
 							 ->
							 basics:append(L2,RestList,List)
							 ;
							 extract_noun_target(Conditions,Predicate,L2),L2 \= []
							 ->
							 basics:append(L2,RestList,List)
							 ;
							 extract_preposition_target(Conditions,Predicate,L2),L2 \= []
                                                         ->
                                                         basics:append(L2,RestList,List)
							 ;
							 List = [Predicate|RestList]
							),
						        extract_target_predicate_helper1(Conditions,RestPredicateList,RestList).


extract_target_predicate_helper2([],[]).
extract_target_predicate_helper2([Predicate|List],PList):- (Predicate = predicate(_,_,_)-_/_
                                                            ->
                                                            PList = [Predicate|RestList]
							    ;
							    Predicate = predicate(_,_,_,_)-_/_
                                  	                    ->
                   					    PList = [Predicate|RestList]
							    ;
							    Predicate = predicate(_,_,_,_,_)-_/_
                                                            ->
                                                            PList = [Predicate|RestList]
							    ;
						  	    PList = RestList
 						   	   ),
							   extract_target_predicate_helper2(List,RestList). 

extract_property_target([],_,[]).
extract_property_target([Predicate|RestPredicate],
                 predicate(V1,be,V2,X)-V3/V4,List) :- (Predicate = property(Y,_,_)-_/_, Y == X
						       -> 
   						       List = [Predicate|RestList]
						       ;
						       List = RestList
						      ),
						      extract_property_target(RestPredicate,predicate(V1,be,V2,X)-V3/V4,RestList).

extract_noun_target([],_,[]).
extract_noun_target([Predicate|RestPredicate],
                    predicate(V1,be,V2,X)-V3/V4,List) :- (Predicate = object(Y,_,_,_,_,_)-_/_, Y == X
                                                          ->
                                                          List = [Predicate|RestList]
                                                          ;
                                                          List = RestList
                                                         ),
                                                         extract_noun_target(RestPredicate,predicate(V1,be,V2,X)-V3/V4,RestList).

extract_preposition_target([],_,[]).
extract_preposition_target([Predicate|RestPredicate],
                           predicate(X,be,V1)-V3/V4,List) :- (Predicate = modifier_pp(Y,_,_)-_/_, Y == X
                                                    	      ->
                                                       	      List = [Predicate|RestList]
                                                       	      ;
                                                       	      List = RestList
                                                             ),
                                                      	     extract_preposition_target(RestPredicate,predicate(X,be,V1)-V3/V4,RestList).
