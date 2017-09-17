:- import append/3 from basics.
:- import numbervars/1 from num_vars.

extract_frame_and_serialize(SentenceID) :-
    open('tmp/query_output.txt',write,Stream),
    close(Stream),
    findall(Predicate-SentenceID/WordID,serialized_drs_fact(Predicate,SentenceID/WordID),DRSFacts),
    numbervars(DRSFacts),
    drs_facts_to_index_list(DRSFacts,DRSIndexList),
    extract_frame_word_by_word(DRSFacts,DRSIndexList,FrameList),
    framelist_to_text(FrameList).

drs_facts_to_index_list([_-Index|Rest],[Index|RestIndex]) :-
    drs_facts_to_index_list(Rest,RestIndex).

drs_facts_to_index_list([],[]).

extract_frame_word_by_word(DRSFacts,[Index|Rest],FrameList) :-
    findall(Frame,fe_extractor_from_drs_facts(DRSFacts,Index,Frame),FList1),
    extract_frame_word_by_word(DRSFacts,Rest,FList2),
    append(FList1,FList2,FrameList). 

extract_frame_word_by_word(_,[],[]).

framelist_to_text([]).

framelist_to_text([Frame|Rest]) :-
    open('tmp/query_output.txt',append,Stream),
    framelist_to_text_helper(Stream,[Frame|Rest]),
    close(Stream).


framelist_to_text_helper(Stream,[frame_tuple(FrameName,FEList)|Rest]) :-
    fmt_write(Stream,"%S\n",FrameName),
    felist_to_text(Stream,FEList),
    framelist_to_text_helper(Stream,Rest).

framelist_to_text_helper(_,[]).

clear_query_output :- 
    open('tmp/query_output.txt',write,Stream),
    close(Stream).

felist_to_text(Stream,[pair(FEName,FE,Index,PredicateName)|Rest]) :-
    Index = SentenceID/WordID,
    pos(PredicateName,POS),
    fmt_write(Stream,"%S=%S=w%S.%S=%S\n",args(FEName,FE,SentenceID,WordID,POS)),
    felist_to_text(Stream,Rest).

felist_to_text(_,[]).
