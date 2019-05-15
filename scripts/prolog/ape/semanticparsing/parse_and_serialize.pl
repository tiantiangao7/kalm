parse_and_serialize(Sentence) :-
    acetext_to_drs(Sentence,_,_,drs(Refs,DRSFacts),_,_),
    Refs \= [],
    DRSFacts \= [],
    !,
    open('tmp\\serialized_drs_fact.txt',write,Stream),
    close(Stream),
    open('tmp\\implicit_var.txt',write,Stream2),
    query_rewrite(Stream2,DRSFacts,DRSFacts2),
    close(Stream2),
    num_vars:numbervars(drs(Refs,DRSFacts2)),
    rewrite_what_query(DRSFacts,DRSFacts2,DRSFacts3),
    num_vars:unnumbervars(DRSFacts3,DRSFacts4),
    num_vars:numbervars(DRSFacts4),
    serialize_drs(DRSFacts4).

parse_and_serialize(Sentence) :-
    acetext_to_drs(Sentence,_,_,_,M,_),
    !,
    open('tmp\\serialized_drs_fact.pl',write,Stream0),
    write(Stream0,'serialized_drs_fact(na,na).\n'),
    close(Stream0),
    open('tmp\\serialized_drs_fact.txt',write,Stream),
    fmt_write(Stream,"%S",args(M)),
    close(Stream),
    open('tmp\\implicit_var.txt',write,Stream2),
    close(Stream2).

parse_and_serialize(Sentence) :-
    \+ acetext_to_drs(Sentence,_,_,_,_,_),
    open('tmp\\serialized_drs_fact.pl',write,Stream0),
    write(Stream0,'serialized_drs_fact(na,na).\n'),
    close(Stream0),
    open('tmp\\serialized_drs_fact.txt',write,Stream),
    write(Stream,'Sentence not accepted by the parser.'),
    close(Stream),
    open('tmp\\implicit_var.txt',write,Stream2),
    close(Stream2).

serialize_drs(DRSFacts) :-
    open('tmp\\serialized_drs_fact.pl',write,Stream),
    serialize_drs_facts(Stream,DRSFacts),
    close(Stream).

serialize_drs_facts(_,[]).
serialize_drs_facts(Stream,[Fact|Rest]) :-
    Fact = Predicate-Index,
    fmt_write(Stream,"serialized_drs_fact(%S,%S).\n",args(Predicate,Index)),
    serialize_drs_facts(Stream,Rest).

query_rewrite(_,[],[]).
query_rewrite(Stream,[Predicate-SentenceID/WordID|Rest],
    [NewPredicate-SentenceID/WordID|NewRest]) :-
    Predicate = query(X,who),
    !,
    NewPredicate = object(X,who,countable,na,eq,1),
    fmt_write(Stream,"w%S.%S\n",args(SentenceID,WordID)),
    query_rewrite(Stream,Rest,NewRest).
    
query_rewrite(Stream,[Predicate-SentenceID/WordID|Rest],
    [NewPredicate-SentenceID/WordID|NewRest]) :-
    Predicate = query(X,what),
    !,
    NewPredicate = object(X,what,countable,na,eq,1),
    fmt_write(Stream,"w%S.%S\n",args(SentenceID,WordID)),
    query_rewrite(Stream,Rest,NewRest).
    
query_rewrite(Stream,[Predicate-SentenceID/WordID|Rest],
    [NewPredicate1-SentenceID/WordID,NewPredicate2-SentenceID/WordID2|NewRest]) :-
    Predicate = query(X,where),
    !,
    NewPredicate1 = modifier_pp(X,loc,Y),
    string:concat_atom([WordID,x],WordID2),
    NewPredicate2 = object(Y,where,countable,na,eq,1),
    fmt_write(Stream,"w%S.%S\n",args(SentenceID,WordID2)),
    query_rewrite(Stream,Rest,NewRest).

query_rewrite(Stream,[Predicate-SentenceID/WordID|Rest],
    [NewPredicate1-SentenceID/WordID,NewPredicate2-SentenceID/WordID2|NewRest]) :-
    Predicate = query(X,when),
    !,
    NewPredicate1 = modifier_pp(X,time,Y),
    string:concat_atom([WordID,x],WordID2),
    NewPredicate2 = object(Y,when,countable,na,eq,1),
    fmt_write(Stream,"w%S.%S\n",args(SentenceID,WordID2)),
    query_rewrite(Stream,Rest,NewRest).

query_rewrite(Stream,[Predicate-_/_|Rest],NewRest) :-
    Predicate = query(_,which),
    !,
    query_rewrite(Stream,Rest,NewRest).
    
query_rewrite(Stream,[Predicate-SentenceID/WordID|Rest],
    [Predicate-SentenceID/WordID|NewRest]) :-
    query_rewrite(Stream,Rest,NewRest).
    
rewrite_what_query(OldDRSFacts,[Predicate-SentenceID/WordID|Rest],
    [NewPredicate-SentenceID/WordID|NewRest]) :-
    Predicate = predicate(C,be,A,B),
    basics:member(query(A,what)-_/_,OldDRSFacts),
    !,
    NewPredicate = predicate(C,be,B,A),
    rewrite_what_query(OldDRSFacts,Rest,NewRest).

% rewrite whose -> of who.
rewrite_what_query(OldDRSFacts,[Predicate-SentenceID/WordID|Rest],
    [NewPredicate-SentenceID/WordID|NewRest]) :-
    Predicate = object(X,what,countable,na,eq,1),
    basics:member(query(X,what)-SentenceID/WordID,OldDRSFacts),
    basics:member(relation(_,of,X)-SentenceID/WordID,OldDRSFacts),
    !,
    NewPredicate = object(X,who,countable,na,eq,1),
    rewrite_what_query(OldDRSFacts,Rest,NewRest).    

% rewrite whose -> of who. rename word index for of in this case.
rewrite_what_query(OldDRSFacts,[Predicate-SentenceID/WordID|Rest],
    [Predicate-SentenceID/WordID2|NewRest]) :-
    Predicate = relation(_,of,A),
    basics:member(query(A,what)-SentenceID/WordID,OldDRSFacts),
    !,
    string:concat_atom([WordID,x],WordID2),
    rewrite_what_query(OldDRSFacts,Rest,NewRest).

% associate what with a type.
rewrite_what_query(OldDRSFacts,[Predicate-SentenceID/WordID|Rest],
    [NewPredicate-SentenceID/WordID|NewRest]) :-
    Predicate = object(X,what,countable,na,eq,1),
    basics:member(predicate(_,be,X,Y)-_/_,OldDRSFacts),
    basics:member(object(Y,Lexem,_,_,_,_)-_/_,OldDRSFacts),
    Y \= named(Lexem),
    Y \= string(Lexem),
    !,
    rewrite_what_query_type_helper(Lexem,Type),
    NewPredicate = object(X,Type,countable,na,eq,1),
    rewrite_what_query(OldDRSFacts,Rest,NewRest).    

% associate what with a type.
rewrite_what_query(OldDRSFacts,[Predicate-SentenceID/WordID|Rest],
    [NewPredicate-SentenceID/WordID|NewRest]) :-
    Predicate = object(X,what,countable,na,eq,1),
    basics:member(predicate(_,be,Y,X)-_/_,OldDRSFacts),
    basics:member(object(Y,Lexem,_,_,_,_)-_/_,OldDRSFacts),
    Y \= named(Lexem),
    Y \= string(Lexem),
    !,
    rewrite_what_query_type_helper(Lexem,Type),
    NewPredicate = object(X,Type,countable,na,eq,1),
    rewrite_what_query(OldDRSFacts,Rest,NewRest).

rewrite_what_query_type_helper(Lexem,Lexem).
 
% associate a which-query with a word index
rewrite_what_query(OldDRSFacts,[Predicate-SentenceID/WordID|Rest],
    [Predicate-SentenceID/WordID|NewRest]) :-
    Predicate = object(X,_,_,_,_,_),
    basics:member(query(X,which)-SentenceID/_,OldDRSFacts),
    !,
    %NewPredicate1 = relation(X,of,Y),
    %NewPredicate2 = object(Y,what,countable,na,eq,1),
    %string:concat_atom([WordID,y],WordID1),
    %string:concat_atom([WordID,yy],WordID2),
    open('tmp\\implicit_var.txt',append,Stream),
    fmt_write(Stream,"w%S.%S\n",args(SentenceID,WordID)),
    %fmt_write(Stream,"w%S.%S\n",args(SentenceID,WordID2)),
    close(Stream),
    rewrite_what_query(OldDRSFacts,Rest,NewRest). 

% add word index for quantity information
rewrite_what_query(OldDRSFacts,[Predicate-SentenceID/WordID|Rest],
    [Predicate-SentenceID/WordID|NewRest]) :-
    Predicate = object(X,_,_,_,_,_),
    basics:member(query(X,howm)-SentenceID/_,OldDRSFacts),
    !,
    open('tmp\\implicit_var.txt',append,Stream),
    fmt_write(Stream,"qw%S.%S\n",args(SentenceID,WordID)),
    close(Stream),
    rewrite_what_query(OldDRSFacts,Rest,NewRest).
/*    
rewrite_what_query(OldDRSFacts,[Predicate-SentenceID/WordID|Rest],
    [Predicate-SentenceID/WordID,NewPredicate-SentenceID/WordID4|NewRest]) :-
    Predicate = object(X,what,countable,na,eq,1),
    basics:member(relation(Y,of,X)-SentenceID/WordID2,OldDRSFacts),
    WordID \= WordID2,
    basics:member(object(Y,Lexem,countable,na,eq,1)-SentenceID/WordID3,OldDRSFacts),
    WordID \= WordID3,
    WordID2 \= WordID3,
    !,
    rewrite_what_query_type_helper(Lexem,Type),
    NewPredicate = object(X,Type,countable,na,eq,1),
    string:concat_atom([WordID,x],WordID4),
    rewrite_what_query(OldDRSFacts,Rest,NewRest). 
*/

rewrite_what_query(OldDRSFacts,[Predicate-SentenceID/WordID|Rest],
    [Predicate-SentenceID/WordID|NewRest]) :-
    rewrite_what_query(OldDRSFacts,Rest,NewRest).

rewrite_what_query(_,[],[]).