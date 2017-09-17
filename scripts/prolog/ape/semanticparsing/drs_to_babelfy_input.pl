drs_to_babelfy_input(DRSFacts) :-
    open('tmp/babelfy_input.txt',write,Stream),
    drs_facts_to_keyval_pair(DRSFacts,NewDRSFacts),
    keysort(NewDRSFacts,NewDRSFacts2),
    predicate_to_babelfy_text(Stream,NewDRSFacts2),
    close(Stream).

drs_facts_to_keyval_pair([],[]).
drs_facts_to_keyval_pair([Predicate-_/WordID|Rest],
    [WordID-Predicate|NewRest]) :- drs_facts_to_keyval_pair(Rest,NewRest).

predicate_to_babelfy_text(_,[]).
predicate_to_babelfy_text(Stream,[Fact|Rest]) :-
    Fact = WordID-Predicate,
    Predicate =.. [PredicateName,_,Lexem|_],
    (PredicateName == has_part
     ->
     true
     ;
     PredicateName == relation
     ->
     true
     ;
     pos(PredicateName,POS),
     fmt_write(Stream,"%S=%S=%S\n",args(Lexem,POS,WordID))
    ),
    predicate_to_babelfy_text(Stream,Rest).


pos(predicate,v).
pos(object,n).
pos(property,a).
pos(modifier_pp,r).
pos(modifier_adv,r).
pos(named,ne).
