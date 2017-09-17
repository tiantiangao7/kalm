:- import numbervars/1 from num_vars.
:- import member/2 from basics.
:- import term_to_atom/2 from string.

% annotate_sentence(+Sentence,+TargetIndex,+Frame,+[pair(FE1,Index1),pair(FE2,Index2),...],-LVP)
% @param Sentence is the CNL sentence to annotate
% @param TargetIndex is the index of the target word
% @param Frame is the frame name
% @param FE_{i} is the frame element name
% @param Index_{i} is the index for each frame element
% @param LVP is of the form lvp(Target,Frame,[pair(FE1,LFE1),pair(FE2,LFE2),...]) where LFE
%        denotes the syntactic pattern for extracting the FEI{i} given Target

annotate_sentence(Sentence,TargetIndex,Frame,FEList,LVP) :-
    acetext_to_drs_training(Sentence,_,_,drs(Refs,Predicates),_,CorefList),
    Refs \= [],
    Predicates \= [],
    !,
    numbervars(drs(Refs,Predicates)),
    get_lfe_from_felist(Predicates,TargetIndex,FEList,LFEList,CorefList),
    get_target_from_index(Predicates,TargetIndex,Target),
    get_predicate_from_word_index(Predicates,TargetIndex,TargetPredicate), 
    get_predicate_type(TargetPredicate,POS),
    LVP = lvp(Target,POS,Frame,LFEList),
    (sentence_annotation(Sentence,TargetIndex,POS,Frame,FEList,LVP)
     ->
     true
    ;
     serialize_sentence_annotation(Sentence,TargetIndex,POS,Frame,FEList,LVP),
     assert(sentence_annotation(Sentence,TargetIndex,POS,Frame,FEList,LVP))
    ),
    (lvp(Target,POS,Frame,LFEList)
     ->
     true
    ;
     serialize_lvp(Target,POS,Frame,LFEList),
     assert(lvp(Target,POS,Frame,LFEList))
    ).

annotate_sentence(Sentence,_,_,_,_) :-
    acetext_to_drs_training(Sentence,_,_,_,M,_),
    write(M). 

get_lfe_from_felist(_,_,[],[],_).

get_lfe_from_felist(DRSPredicates,TargetIndex,[pair(FE,FEIndex)|Rest],
    [pair(FE,LFE_Atom)|LFEList],CorefList):-
    member(coref(FEIndex,CorefIndex),CorefList),
    !,
    construct_lfe(DRSPredicates,TargetIndex,CorefIndex,LFE),
    term_to_atom(LFE,LFE_Atom),
    (logical_syntactic_pattern(LFE_Atom)
     ->
     true
    ;
     serialize_lfe(LFE),
     assert(logical_syntactic_pattern(LFE_Atom))
    ),
    get_lfe_from_felist(DRSPredicates,TargetIndex,Rest,LFEList,CorefList). 
  
get_lfe_from_felist(DRSPredicates,TargetIndex,[pair(FE,FEIndex)|Rest],
    [pair(FE,LFE_Atom)|LFEList],CorefList):-
    construct_lfe(DRSPredicates,TargetIndex,FEIndex,LFE),
    term_to_atom(LFE,LFE_Atom),
    (logical_syntactic_pattern(LFE_Atom)
     ->
     true
    ;
     serialize_lfe(LFE),
     assert(logical_syntactic_pattern(LFE_Atom))
    ),
    get_lfe_from_felist(DRSPredicates,TargetIndex,Rest,LFEList,CorefList).    

get_target_from_index(DRSPredicates,TargetIndex,Target) :-
    member(Predicate-TargetIndex,DRSPredicates),
    Predicate =.. [_,_,Target|_].
    
serialize_sentence_annotation(Sentence,TargetIndex,POS,Frame,FEList,
    lvp(Target,POS,Frame,LFEList)) :-
    open('semanticparsing/data/sentence_annotation.pl',append,Stream),
    fmt_write(Stream,"%S(\'%S\',%S,\'%S\',\'%S\',",
        args(sentence_annotation,Sentence,TargetIndex,POS,Frame)),
    write(Stream,'['),
    serialize_FEList(Stream,FEList),
    write(Stream,'],'),
    fmt_write(Stream,"%S(\'%S\',\'%S\',\'%S\',",
        args(lvp,Target,POS,Frame)),
    write(Stream,'['),
    serialize_LFEList(Stream,LFEList),
    write(Stream,'])).\n'),
    close(Stream).
    
serialize_FEList(Stream,[pair(FE1,Index1),pair(FE2,Index2)|Rest]) :-
    fmt_write(Stream,"%S(\'%S\',%S),",args(pair,FE1,Index1)),
    serialize_FEList(Stream,[pair(FE2,Index2)|Rest]).
    
serialize_FEList(Stream,[pair(FE1,Index1)]) :-
    fmt_write(Stream,"%S(\'%S\',%S)",args(pair,FE1,Index1)).
    
serialize_lvp(Target,POS,Frame,LFEList) :-
    open('semanticparsing/data/lvp.pl',append,Stream),
    fmt_write(Stream,"%S(\'%S\',\'%S\',\'%S\',",
        args(lvp,Target,POS,Frame)),
    write(Stream,'['),
    serialize_LFEList(Stream,LFEList),
    write(Stream,']).\n'),
    close(Stream).

serialize_LFEList(Stream,[pair(FE1,LFE1),pair(FE2,LFE2)|Rest]) :-
    fmt_write(Stream,"%S(\'%S\',\'%S\'),",args(pair,FE1,LFE1)),
    serialize_LFEList(Stream,[pair(FE2,LFE2)|Rest]).

serialize_LFEList(Stream,[pair(FE1,LFE1)]) :-
    fmt_write(Stream,"%S(\'%S\',\'%S\')",args(pair,FE1,LFE1)).
    
?- retractall(logical_syntactic_pattern(_)),retractall(lvp(_)),
   retractall(sentence_annotation(_,_,_,_,_)).