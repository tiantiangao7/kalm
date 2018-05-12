:- import append/3,length/2 from basics.
:- import numbervars/1 from num_vars.
:- import concat_atom/2 from string.

:- dynamic(gl_sk_counter/1).

drs_to_framenet_logical_form(Sentence,TargetIndex,LogicalForm) :-
    retractall(gl_sk_counter(_)),
    assert(gl_sk_counter(0)),
    fe_extractor(Sentence,TargetIndex,frame_tuple(FrameName,FEList)),
    get_new_sk_constant(FrameSkolem),
    felist_to_logical_form(FrameName,FrameSkolem,FEList,LogicalForm).

get_new_sk_constant(Skolem_Constant) :-
   gl_sk_counter(X),
   Y is X + 1,
   concat_atom([sk,Y],Skolem_Constant),
   retractall(gl_sk_counter(_)),
   assert(gl_sk_counter(Y)).

felist_to_logical_form(FrameName,FrameSkolem,[pair(FEName,FEVal)|Rest],[P1|RestLF]) :-
    Arg1 =.. [FEName,FrameSkolem],
    number(FEVal),
    !,
    P1 =.. [FrameName,Arg1,num(FEVal)],
    felist_to_logical_form(FrameName,FrameSkolem,Rest,RestLF).

felist_to_logical_form(FrameName,FrameSkolem,[pair(FEName,FEVal)|Rest],[P1,P2|RestLF]) :-
    Arg1 =.. [FEName,FrameSkolem],
    get_new_sk_constant(Arg2),
    P1 =.. [FrameName,Arg1,Arg2],
    P2 =.. [FEVal,Arg2],
    felist_to_logical_form(FrameName,FrameSkolem,Rest,RestLF).

felist_to_logical_form(_,_,[],[]).

fe_extractor_from_drs_facts(DRSFacts,TargetIndex,FrameTuple) :-
    numbervars(drs(_,DRSFacts)),
    get_predicate_from_word_index(DRSFacts, TargetIndex, TargetPredicate-_),
    TargetPredicate =.. [Type,_,Lexem|_],
    lvp(Lexem,Type,FrameName,FE_Logical_Syntactic_Pattern_List),
    extract_fe_list_from_lvp(DRSFacts,TargetIndex,FE_Logical_Syntactic_Pattern_List,
    FEList),
    FrameTuple = frame_tuple(FrameName,FEList),
    length(FEList,Len),
    Len > 1.

fe_extractor(Sentence,TargetIndex,FrameTuple) :-
    acetext_to_drs(Sentence,_,_,drs(Refs,DRSFacts),_,_),
    Refs \= [],
    DRSFacts \= [],
    !,
    numbervars(drs(Refs,DRSFacts)),
    drs_to_babelfy_input(DRSFacts),
    get_predicate_from_word_index(DRSFacts, TargetIndex, TargetPredicate-_),
    TargetPredicate =.. [Type,_,Lexem|_],
    lvp(Lexem,Type,FrameName,FE_Logical_Syntactic_Pattern_List),
    extract_fe_list_from_lvp(DRSFacts,TargetIndex,FE_Logical_Syntactic_Pattern_List,
    FEList),
    FrameTuple = frame_tuple(FrameName,FEList).    

fe_extractor(Sentence,_,_) :-
    acetext_to_drs(Sentence,_,_,_,M,_),
    write(M).

extract_fe_list_from_lvp(DRSFacts,TargetIndex,[pair(FEName,Logical_Syntactic_Pattern,
    Flag)|Rest],FEList) :-
    extract_fe_from_logical_syntactic_pattern(
    DRSFacts,TargetIndex,Logical_Syntactic_Pattern,FE,FEIndex,PredicateName),
    is_valid_fe_val(FEName,FE),
    L1 = [pair(FEName,FE,FEIndex,PredicateName)],
    extract_fe_list_from_lvp(DRSFacts,TargetIndex,Rest,RestFEList),
    \+ basics:memberchk(pair(_,_,FEIndex,_),RestFEList),
    append(L1,RestFEList,FEList).    

extract_fe_list_from_lvp(DRSFacts,TargetIndex,[pair(FEName,Logical_Syntactic_Pattern,
    optional)|Rest],FEList) :-
    extract_fe_list_from_lvp(DRSFacts,TargetIndex,Rest,FEList).

extract_fe_list_from_lvp(_,_,[],[]).

extract_fe_from_logical_syntactic_pattern(DRSFacts,TargetIndex,Logical_Syntactic_Pattern,
    FE,FEIndex,PredicateNameResult) :-
    apply_pattern_to_target(Logical_Syntactic_Pattern,DRSFacts,TargetIndex,FEIndex),
    get_predicate_from_word_index(DRSFacts, FEIndex, FEPredicate-_),
    (FEPredicate =.. [PredicateName,_,FE,named|_]
     ->
     PredicateNameResult = named
     ;
     FEPredicate =.. [PredicateName,_,FE|_],
     PredicateNameResult = PredicateName
    ).    
