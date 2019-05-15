sentence_to_metaqa_query(Sentence) :-
    retractall(serialized_drs_fact(_,_)),
    parse_and_serialize(Sentence),
    (extract_frame_and_serialize(1)
     ->
     true
     ;
     write(Sentence)
    ).

filter_metaqa_frame_list([frame_tuple(FrameName,FEList)|Rest], FilteredFrameList) :-
	(FrameName == 'Movie', is_valid_metaqa_frame_element_list(FEList)
	 ->
	 filter_metaqa_frame_list(Rest, NewRest),
	 FilteredFrameList = [frame_tuple(FrameName,FEList)|NewRest]
	 ;
	 filter_metaqa_frame_list(Rest,FilteredFrameList)
	).
filter_metaqa_frame_list([],[]).

is_valid_metaqa_frame_element_list([pair(FEName, FE, _, _)|Rest]) :-
	metaqa_fe(FEName, FE),
	is_valid_metaqa_frame_element_list(Rest).
is_valid_metaqa_frame_element_list([]).

framelist_to_metaqa_query([],_) :-
	!,
    open('metaqa\\metaqa_query.txt',write,Stream),
    close(Stream).

framelist_to_metaqa_query(FrameList,DRSFacts) :-
    open('metaqa\\metaqa_query.txt',write,Stream),
    write(Stream,'?-'),
    framelist_to_metaqa_query_helper(Stream,FrameList,DRSFacts),
    write(Stream,'.'),
    close(Stream).

framelist_to_metaqa_query_helper(Stream,
	[frame_tuple(_,FEList),Frame2|Rest],DRSFacts) :-
    write(Stream,'movie('),
    felist_to_metaqa_query(Stream,FEList,DRSFacts),
    write(Stream,'),'),
    framelist_to_metaqa_query_helper(Stream,[Frame2|Rest],DRSFacts).

framelist_to_metaqa_query_helper(Stream,[frame_tuple(_,FEList)],DRSFacts) :-
    write(Stream,'movie('),
    felist_to_metaqa_query(Stream,FEList,DRSFacts),
    write(Stream,')').

felist_to_metaqa_query(Stream,[pair(FEName,FE,Index,PredicateName),FE2|Rest],DRSFacts) :-
    Index = _/WordID,
    pos(PredicateName,POS),
    (get_predicate_from_word_index(DRSFacts,Index,_-Index)
     ->
     true
     ;
     write(Stream,'Error in getting object from index.\n')
    ),
    (POS == ne
     ->
     fmt_write(Stream,"\'%S\',\'%S\',",args(FEName,FE))
     ;
     fmt_write(Stream,"\'%S\',W%S,",args(FEName,WordID))
    ),
    felist_to_metaqa_query(Stream,[FE2|Rest],DRSFacts).

felist_to_metaqa_query(Stream,[pair(FEName,FE,Index,PredicateName)],DRSFacts) :-
    Index = _/WordID,
    pos(PredicateName,POS),
    (get_predicate_from_word_index(DRSFacts,Index,_-Index)
     ->
     true
     ;
     write(Stream,'Error in getting object from index.\n')
    ),
    (POS == ne
     ->
     fmt_write(Stream,"\'%S\',\'%S\'",args(FEName,FE))
     ;
     fmt_write(Stream,"\'%S\',W%S",args(FEName,WordID))
    ).

metaqa_fe(X,Y) :- metaqa_fe_synonym(X,Y).
metaqa_fe(X,Y) :- metaqa_entity_kb(X,Y).

metaqa_fe_synonym('Film','film').
metaqa_fe_synonym('Film','movie').
metaqa_fe_synonym('Actor','who').
metaqa_fe_synonym('Actor','actor').
metaqa_fe_synonym('Actor','person').
metaqa_fe_synonym('Writer','writer').
metaqa_fe_synonym('Writer','screenwriter').
metaqa_fe_synonym('Writer','who').
metaqa_fe_synonym('Writer','person').
metaqa_fe_synonym('Genre','genre').
metaqa_fe_synonym('Genre','type').
metaqa_fe_synonym('Director','director').
metaqa_fe_synonym('Director','who').
metaqa_fe_synonym('Director','person').
metaqa_fe_synonym('Release Year','when').
metaqa_fe_synonym('Release Year','year').
metaqa_fe_synonym('Release Year','release-date').
metaqa_fe_synonym('Release Year','release-year').
metaqa_fe_synonym('Language','language').