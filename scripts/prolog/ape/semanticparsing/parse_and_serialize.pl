parse_and_serialize(Sentence) :-
    acetext_to_drs(Sentence,_,_,drs(Refs,DRSFacts),_,_),
    Refs \= [],
    DRSFacts \= [],
    !,
    num_vars:numbervars(drs(Refs,DRSFacts)),
    open('tmp/serialized_drs_fact.txt',write,Stream),
    close(Stream),
    serialize_drs(DRSFacts).

parse_and_serialize(Sentence) :-
    acetext_to_drs(Sentence,_,_,_,M,_),
    open('tmp/serialized_drs_fact.pl',write,Stream0),
    close(Stream0),
    open('tmp/serialized_drs_fact.txt',write,Stream),
    fmt_write(Stream,"%S",args(M)),
    close(Stream).

serialize_drs(DRSFacts) :-
    open('tmp/serialized_drs_fact.pl',write,Stream),
    serialize_drs_facts(Stream,DRSFacts),
    close(Stream).

serialize_drs_facts(_,[]).
serialize_drs_facts(Stream,[Fact|Rest]) :-
    Fact = Predicate-Index,
    fmt_write(Stream,"serialized_drs_fact(%S,%S).\n",args(Predicate,Index)),
    serialize_drs_facts(Stream,Rest).

