%% count_leaf_node(+List,0,-Count)
%% input is a syntax tree
%% returns the number of leaf nodes (words) of the tree
	
count_leaf_node(SyntaxTree,Offset,WordCount):-
	!,traverse_parent(SyntaxTree,Offset,WordCount).
	
traverse_parent([POS|RestElements],PrevWordCount,NextWordCount):-
	!,atom(POS),traverse_child(RestElements,PrevWordCount,NextWordCount).

traverse_parent([],PrevWordCount,PrevWordCount).
	
traverse_child([Element|RestElements],PrevWordCount,NextWordCount):-
	!,
	(Element = '.'
	->
	CurWordCount is PrevWordCount
	;
	atom(Element)
	->
	tokenize(Element,ElementList),basics:length(ElementList,Len),CurWordCount is PrevWordCount + Len
	;
	is_list(Element)
	->
	traverse_parent(Element,PrevWordCount,CurWordCount)
	;
	writeln('not considered'),CurWordCount is PrevWordCount
	),traverse_child(RestElements,CurWordCount,NextWordCount).
	
traverse_child([],PrevWordCount,PrevWordCount).

get_syntax_tree_from_sent_list(SyntaxTreeList,SentNo,SyntaxTree):-
	SyntaxTreeList = [HeadST|TailST],
	(SentNo == 1
	->
	SyntaxTree = HeadST
	;
	NewSentNo is SentNo - 1,
	get_syntax_tree_from_sent_list(TailST,NewSentNo,SyntaxTree)
	).
	
get_phrase_index_list([specification,[cond_s,if,S1,then,[s_coord,S2|RestSentences]],'.'],_,IndexList):-
	!,count_leaf_node(S1,0,S1WordCount),
	CurLoc is S1WordCount + 2,
	count_leaf_node(S2,0,S2WordCount),
    Start is CurLoc + 1,
	End is CurLoc + S2WordCount, 
	Index = phrase_loc(Start,End),
	NextLoc is End + 1,
	get_phrase_index_list(RestSentences,NextLoc,RestIndexList),
	IndexList = [Index|RestIndexList].
	
get_phrase_index_list([[coord,and],[s_coord,S1|RestSentences]],CurLoc,IndexList):-
	!,count_leaf_node(S1,0,S1WordCount),
	Start is CurLoc + 1,
	End is CurLoc + S1WordCount,
	Index = phrase_loc(Start,End),
	NextLoc is End + 1,
	get_phrase_index_list(RestSentences,NextLoc,RestIndexList),
	IndexList = [Index|RestIndexList].
	
get_phrase_index_list([[coord,and],S1|RestSentences],CurLoc,IndexList):-
	!,count_leaf_node(S1,0,S1WordCount),
	Start is CurLoc + 1,
	End is CurLoc + S1WordCount,
	Index = phrase_loc(Start,End),
	NextLoc is End + 1,
	get_phrase_index_list(RestSentences,NextLoc,RestIndexList),
	IndexList = [Index|RestIndexList].

get_phrase_index_list([],_,[]).

is_sent_conjunction([specification,[cond_s,if,_,then,[s_coord|_]],'.']).

get_phrase_conds(phrase_loc(Start,End),[Condition1|RestConditions],PhraseConditions):-
	!,get_cond_loc(Condition1,[],[Element|_]),
	((Element >= Start, End >= Element)
	->
	PhraseCondition = Condition1,
	get_phrase_conds(phrase_loc(Start,End),RestConditions,RestPhraseConditions),
	PhraseConditions = [PhraseCondition|RestPhraseConditions]
	;
	get_phrase_conds(phrase_loc(Start,End),RestConditions,RestPhraseConditions),
	PhraseConditions = RestPhraseConditions
	).

get_phrase_conds(_,[],[]).
		
get_drs_loc(drs(Refs,Conditions),InList,OutList) :-
  !,get_cond_loc(Conditions,InList,OutList).

get_cond_loc(-Drs,InList,OutList):-
	!,get_drs_loc(Drs,InList,OutList).

get_cond_loc(~Drs,InList,OutList):-
	!,get_drs_loc(Drs,InList,OutList).
		
get_cond_loc(Drs1 => Drs2,InList,OutList):-
	!,get_drs_loc(Drs1,InList,TempList),
	get_drs_loc(Drs2,TempList,OutList).
		
get_cond_loc([Condition],InList,OutList):-
	!,get_cond_loc(Condition,InList,OutList).
	
get_cond_loc([Condition1,Condition2|RestConditions],InList,OutList):-
	!,get_cond_loc(Condition1,InList,TempList),
	get_cond_loc([Condition2|RestConditions],TempList,OutList).

get_cond_loc(BasicCondition - SID/TID, InList,OutList):-!,OutList = [TID|InList].
