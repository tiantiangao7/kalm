is_valid_metaqa_query(X) :- 
	acetext_to_drs(X,_,_,drs(Refs,DRSFacts),_,_),
    Refs \= [],
    DRSFacts \= [],
    !.
    
is_valid_metaqa_query(X) :-
	acetext_to_drs(X,_,_,_,_,_),
    !,
    open('metaqa\\errors.txt', append, Stream),
    write(Stream, X),
    write(Stream,'\n'),
    close(Stream).