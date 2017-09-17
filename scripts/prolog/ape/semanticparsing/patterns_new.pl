get_connection_by_pattern('verb->pp,pp->dep', Conditions, C1, C3) :-
    get_predicate_from_word_index(Conditions, C1, P1),
    get_pp_from_verb(Conditions, P1, C2),
    get_predicate_from_word_index(Conditions, C2, P2),
    get_dep_from_pp(Conditions, P2, C3).

pattern_by_frame_element(buy, location, 'verb->pp,pp->dep').
pattern_by_frame_element(buy, location, 'verb->pp,pp->form,from->dep').

pattern_frame_element_target(F,FE,T,DRS,FE_Word):-
	pattern_by_frame_element(F,FE,Pattern),
	get_word_index(T,T_index),
	get_connection_by_pattern(Pattern,DRS,T_index,FE_word).