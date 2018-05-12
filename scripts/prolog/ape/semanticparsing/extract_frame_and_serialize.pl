:- import append/3, member/2,reverse/2 from basics.
:- import numbervars/1 from num_vars.

extract_frame_and_serialize(SentenceID) :-
    open('tmp/query_output.txt',write,Stream),
    close(Stream),
    findall(Predicate-SentenceID/WordID,serialized_drs_fact(Predicate,SentenceID/WordID),DRSFacts),
    numbervars(DRSFacts),
    drs_facts_to_index_list(DRSFacts,DRSIndexList),
    extract_frame_word_by_word(DRSFacts,DRSIndexList,FrameList),
    remove_subsumed_frame(FrameList,FrameList2),
    reverse(FrameList2,FrameList3),
    remove_subsumed_frame(FrameList3,FrameList4),
    framelist_to_text(FrameList4,DRSFacts).

drs_facts_to_index_list([_-Index|Rest],[Index|RestIndex]) :-
    drs_facts_to_index_list(Rest,RestIndex).

drs_facts_to_index_list([],[]).

extract_frame_word_by_word(DRSFacts,[Index|Rest],FrameList) :-
    findall(Frame,fe_extractor_from_drs_facts(DRSFacts,Index,Frame),FList1),
    extract_frame_word_by_word(DRSFacts,Rest,FList2),
    append(FList1,FList2,FrameList). 

extract_frame_word_by_word(_,[],[]).

framelist_to_text([],_).

framelist_to_text([Frame|Rest],DRSFacts) :-
    open('tmp/query_output.txt',append,Stream),
    framelist_to_text_helper(Stream,[Frame|Rest],DRSFacts),
    close(Stream).


framelist_to_text_helper(Stream,[frame_tuple(FrameName,FEList)|Rest],DRSFacts) :-
    fmt_write(Stream,"%S\n",FrameName),
    felist_to_text(Stream,FEList,DRSFacts),
    framelist_to_text_helper(Stream,Rest,DRSFacts).

framelist_to_text_helper(_,[],_).

clear_query_output :- 
    open('tmp/query_output.txt',write,Stream),
    close(Stream).

felist_to_text(Stream,[pair(FEName,FE,Index,PredicateName)|Rest],DRSFacts) :-
    Index = SentenceID/WordID,
    pos(PredicateName,POS),
    (get_predicate_from_word_index(DRSFacts,Index,Predicate-Index)
     ->
     true
     ;
     write(Stream,'Error in getting object from index.\n')
    ), 
    get_object_quantity(Predicate,Quantity),
    fmt_write(Stream,"%S=%S=w%S.%S=%S=%S\n",args(FEName,FE,SentenceID,WordID,POS,Quantity)),
    felist_to_text(Stream,Rest,DRSFacts).

felist_to_text(_,[],_).

frame_subsumption(frame_tuple(FN,[FE1|Rest1]),frame_tuple(FN,FEList2)) :-
    member(FE1,FEList2),
    frame_subsumption(frame_tuple(FN,Rest1),frame_tuple(FN,FEList2)).    
frame_subsumption(frame_tuple(FN,[]),frame_tuple(FN,_)).

frame_subsumption_member(F1,[F2|_]) :-
    frame_subsumption(F1,F2),
    !.
frame_subsumption_member(F1,[_|Rest]) :-
    frame_subsumption_member(F1,Rest).

remove_subsumed_frame([F1],[F1]).
remove_subsumed_frame([F1,F2|Rest],FList) :-
    (frame_subsumption_member(F1,[F2|Rest])
     ->
     remove_subsumed_frame([F2|Rest],FList)
     ;
     remove_subsumed_frame([F2|Rest],FList2),
     FList = [F1|FList2]
    ).

get_object_quantity(property(_,_,_),unknown).
get_object_quantity(property(_,_,_,_),unknown).
get_object_quantity(property(_,_,_,_,_,_),unknown).

get_object_quantity(object(_,_,_,Unit,Op,Count),Quantity) :-
    (Unit \== na, Count \== na
     -> 
     object_quantity_constraint_to_text(Op,QuantityConstraint),
     string:concat_atom([QuantityConstraint,Count,' ',Unit],Quantity)
     ;
     Count \== na
     ->
     object_quantity_constraint_to_text(Op,QuantityConstraint),
     string:concat_atom([QuantityConstraint,Count],Quantity)
     ;
     Quantity = unknown
    ).

object_quantity_constraint_to_text(Op,QuantityConstraint) :-
    (Op == geq
     ->
     QuantityConstraint = 'greater or equal to '
     ;
     Op == greater
     ->
     QuantityConstraint = 'greater than '
     ;
     Op == leq
     ->
     QuantityConstraint = 'less or equal to '
     ;
     Op == less
     ->
     QuantityConstraint = 'less than '
     ;
     QuantityConstraint = ''
    ).