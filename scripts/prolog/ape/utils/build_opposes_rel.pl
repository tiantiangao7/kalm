:- op(400,  xfx, :).           % label
:- op(400,  fy, -).            % negation
:- op(400,  fy, ~).            % negation as failure
:- op(400,  fy, can).          % possibility 
:- op(400,  fy, must).         % necessity
:- op(400,  fy, may).          % admission 
:- op(400,  fy, should).       % recommendation
:- op(500, xfy, &).            % conjunction
:- op(600, xfy, v).            % disjunction
:- op(650, xfy, =>).           % implication 

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
	
%% get_predicate_index_from_sentence(+SyntaxTree,-PositionList)
%% input is the syntax tree of a sentence
%% returns the a list of indices of the predicates
	
get_predicate_position_from_simple_sentence([s,NP,VP],Offset,Position):-	
	count_leaf_node(NP,Offset,NPWordCount),
	search_parent(VP,NPWordCount,_,VerbPos,CopulaPos),
	(nonvar(VerbPos)
	->
	Position = VerbPos
	;
	nonvar(CopulaPos)
	->
	Position = CopulaPos
	;
	true
	).

get_predicate_positions_from_sentence_coord([s,NP,VP],Offset,[Position]):-	
	get_predicate_position_from_simple_sentence([s,NP,VP],Offset,Position).	
	
get_predicate_positions_from_sentence_coord([s_coord,S1|RestSentences],Offset,[Pos1|RestPos]):-
	!,get_predicate_position_from_simple_sentence(S1,Offset,Pos1),
	count_leaf_node(S1,0,S1WordCount),
	CurOffset is S1WordCount + Offset,
	get_predicate_positions_from_sentence_coord(RestSentences,CurOffset,RestPos).
	
get_predicate_positions_from_sentence_coord([[coord,and],Sentence_Coord],Offset,PositionList):-
	!,CurOffset is Offset + 1,
	get_predicate_positions_from_sentence_coord(Sentence_Coord,CurOffset,PositionList).
	
get_predicate_positions_from_sentence_coord([],_,[]).
	
%% simple sentence: Subject + Predicate + Complement 	
get_predicate_index_from_sentence([specification,[s,NP,VP],'.'],PositionList):-
	!,get_predicate_positions_from_sentence_coord([s,NP,VP],0,PositionList).
		
%% sentence coordinations
get_predicate_index_from_sentence([specification,[s_coord|Sentences],'.'],PositionList):-
		!,get_predicate_positions_from_sentence_coord([s_coord|Sentences],0,PositionList).
		
%% if + simple sentence + then + simple sentence or sentence coordination
get_predicate_index_from_sentence([specification,[cond_s,if,S1,then,S2],'.'],PositionList):-
		!,count_leaf_node(S1,0,S1WordCount),
		CurWordCount is S1WordCount + 2,
		get_predicate_positions_from_sentence_coord(S2,CurWordCount,PositionList).

%% topicalised sentence
get_predicate_index_from_sentence([specification,[top_s,Topic,Sentence],'.'],PositionList):-
		!,count_leaf_node(Topic,0,TopicWordCount),
		get_predicate_positions_from_sentence_coord(Sentence,TopicWordCount,PositionList).
			
			
search_parent([POS|RestElements],PrevWordCount,NextWordCount,VerbPos,CopulaPos):-
	!,atom(POS),
	(POS == v
	->
	VerbPos is PrevWordCount + 1,
	search_child(RestElements,PrevWordCount,NextWordCount,_,_)
	;
	POS == aux
	->
	CopulaPos is PrevWordCount + 1,
	search_child(RestElements,PrevWordCount,NextWordCount,_,_)
	;
	search_child(RestElements,PrevWordCount,NextWordCount,VerbPos,CopulaPos)
	).

search_parent([],PrevWordCount,PrevWordCount,_,_).

search_child([Element|RestElements],PrevWordCount,NextWordCount,VerbPos,CopulaPos):-
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
	search_parent(Element,PrevWordCount,CurWordCount,VerbPos1,CopulaPos1)
	;
	writeln('not considered'),CurWordCount is PrevWordCount
	),
	search_child(RestElements,CurWordCount,NextWordCount,VerbPos2,CopulaPos2),
	(nonvar(VerbPos1)
	->
	VerbPos = VerbPos1
	;
	nonvar(VerbPos2)
	->
	VerbPos = VerbPos2
	;
	true
	),
	(nonvar(CopulaPos1)
	->
	CopulaPos = CopulaPos1
	;
	nonvar(CopulaPos2)
	->
	CopulaPos = CopulaPos2
	;
	true
	).
	
search_child([],PrevWordCount,PrevWordCount,_,_).

			
	
%% get_predicate_cond(+ConditionList,+WordIndex,-PredCondition):-
%% Input ConditionList is a list of simple conditions or an implication drs.
%% return the simple condition with the specified Token ID	
%% BUG: If Mary has a dog then every bird flies.

%get_predicate_cond([drs(Refs1,Conds1) => drs([],[-drs(Refs2,Conds2)])],WordIndex,PredCondition):-
%	!,extract_predicate_cond_from_list(Conds2,WordIndex,PredCondition).
	
%get_predicate_cond([drs(Refs1,Conds1) => drs(Refs2,Conds2)],WordIndex,PredCondition):-
%	!,extract_predicate_cond_from_list(Conds2,WordIndex,PredCondition).

%get_predicate_cond(CondsList,WordIndex,PredCondition):-
%	!,extract_predicate_cond_from_list(CondsList,WordIndex,PredCondition).	
	
%extract_predicate_cond_from_list([Condition-SID/TID|RestConds],TID,Condition-SID/TID):-!.
%extract_predicate_cond_from_list([Cond|RestConds],TID,PredicateCondition):-
%	extract_predicate_cond_from_list(RestConds,TID,PredicateCondition).	
	
%copy_list([Term],[CopiedTerm]):-!,copy_term(Term,CopiedTerm).
%copy_list([Term|RestTerms],[CopiedTerm|RestCopiedTerms]):-!,copy_term(Term,CopiedTerm),copy_list(RestTerms,RestCopiedTerms).

get_predicate_from_drs(drs(Refs,Conds), PositionList, PredicateList) :-
  !,
  get_predicate_from_condition_list(Conds, PositionList, PredicateList).

get_predicate_from_condition_list([Condition - SID/TID|RestConditions], PositionList, PredicateList):-
	!,
	(basics:member(TID,PositionList)
	->
	PredicateList = [Condition - SID/TID|RestPredicateElements]
	;
	PredicateList = RestPredicateElements
	),
	get_predicate_from_condition_list(RestConditions,PositionList,RestPredicateElements).

get_predicate_from_condition_list([drs(Refs,Conds)|RestConditions], PositionList, PredicateList):-
	!,get_predicate_from_condition_list(Conds,PositionList,PredicateElements),
	get_predicate_from_condition_list(RestConditions,PositionList,RestPredicateElements),
	basics:append(PredicateElements,RestPredicateElements,PredicateList).
	
get_predicate_from_condition_list([-DRS|RestConditions], PositionList, PredicateList):-
	!,get_predicate_from_drs(DRS,PositionList,PredicateElements),
	get_predicate_from_condition_list(RestConditions,PositionList,RestPredicateElements),
	basics:append(PredicateElements,RestPredicateElements,PredicateList).

get_predicate_from_condition_list([can(DRS)|RestConditions], PositionList, PredicateList):-
	!,get_predicate_from_drs(DRS,PositionList,PredicateElements),
	get_predicate_from_condition_list(RestConditions,PositionList,RestPredicateElements),
	basics:append(PredicateElements,RestPredicateElements,PredicateList).

get_predicate_from_condition_list([must(DRS)|RestConditions], PositionList, PredicateList):-
	!,get_predicate_from_drs(DRS,PositionList,PredicateElements),
	get_predicate_from_condition_list(RestConditions,PositionList,RestPredicateElements),
	basics:append(PredicateElements,RestPredicateElements,PredicateList).

get_predicate_from_condition_list([World:DRS|RestConditions], PositionList, PredicateList):-
	!,get_predicate_from_drs(DRS,PositionList,PredicateElements),
	get_predicate_from_condition_list(RestConditions,PositionList,RestPredicateElements),
	basics:append(PredicateElements,RestPredicateElements,PredicateList).
		
get_predicate_from_condition_list([DRS1 => DRS2|RestConditions], PositionList, PredicateList):-
	!,get_predicate_from_drs(DRS1,PositionList,PredicateElements1),
	get_predicate_from_drs(DRS2,PositionList,PredicateElements2),
	basics:append(PredicateElements1,PredicateElements2,PredicateElements),
	get_predicate_from_condition_list(RestConditions,PositionList,RestPredicateElements),
	basics:append(PredicateElements,RestPredicateElements,PredicateList).
	
get_predicate_from_condition_list([], _, []).


%% get_labelled_drs(+RuleLabel,+DRS,-List)
%% input DRS is the result returned by the attempto parser
%% the returned result can be either an implication, drs1 => drs2, or a list a basic conditions
get_labelled_drs(RuleLabel,[],[]):-!.
get_labelled_drs(RuleLabel,[RuleLabel(Cond)|RestConds],[Cond|RestLabelledConds]):-!,get_labelled_drs(RuleLabel,RestConds,RestLabelledConds).
get_labelled_drs(RuleLabel,[Cond|RestConds],RestLabelledConds):-!,get_labelled_drs(RuleLabel,RestConds,RestLabelledConds).
get_labelled_drs(RuleLabel,drs(Refs,Conds),CopiedCondsList):-!,get_labelled_drs(RuleLabel,Conds,CondsList),copy_term(list(CondsList),list(CopiedCondsList)).

%% get_sentence_syntaxtree(+SentenceList,+ID,-ST)
%% SentenceList is the syntax tree returned by the attempto parser
get_sentence_syntaxtree(SentenceList,ID,ST):-basics:ith(ID,SentenceList,ST).

%% get_opposes_cond(+SyntaxTreeList,+DRS,+r(ID),-LabelledRuleCondsList,-PredCondition)
%% SyntaxTreeList,DRS are the results returned by the attempto parser
%% output the labelled drs-conditions and the predicate condition
get_opposes_drs(SentenceList,DRS,r(ID),LabelledRuleCondsList,PredicateList):-
	get_sentence_syntaxtree(SentenceList,ID,SyntaxTree),
	get_predicate_index_from_sentence(SyntaxTree,PositionList),
	get_labelled_drs(r(ID),DRS,LabelledRuleCondsList),
	get_predicate_from_condition_list(LabelledRuleCondsList,PositionList,PredicateList).



get_subject_var_from_plist([Condition-SID/TID|RestConditions],[SubjRef|RestSubjRefs]):-
	!,Condition =.. [Func,Ref,Verb,SubjRef|_],
	get_subject_var_from_plist(RestConditions,RestSubjRefs).	

get_subject_var_from_plist([],[]).	

%% /
	
%% labelled rule is 1) [drs1=>drs2] or  2) [cond1, cond2, cond3, ... condn]
%% do we allow [drs1=>drs2] refutes [cond1, cond2, cond3, ... condn]???	


%% find_common_subject_vars(+ForallVars,+HeadSubjectVars,-CommonSubjectVars)
find_common_subject_vars(ForallVars,[HeadSubjectVar|RestHeadSubjectVars],VarList):-
	!,
	(basics:member(HeadSubjectVar,ForallVars)
	->
	VarList = [HeadSubjectVar|RestCommonSubjectVars]
	;
	VarList = RestCommonSubjectVars
	),
	find_common_subject_vars(ForallVars,RestHeadSubjectVars,RestCommonSubjectVars).
	
find_common_subject_vars(ForallVars,[],[]).
	
%%	cartesian_product_head_predicate(+Head1,+Head2,+VarList1,+VarList2,-OpposesList)
cartesian_product_head_predicate(R1,Head1,[Var1],R2,Head2,[Var2],[opposes(R1,NewHead1,R2,NewHead2)]):-
	!,Head1 =.. [Functor1|Args1],
	cond_var_to_ergo_var(Args1,NewArgs1,Var1),
	NewHead1 =.. [Functor1|NewArgs1],
	Head2 =.. [Functor2|Args2],
	cond_var_to_ergo_var(Args2,NewArgs2,Var2),
	NewHead2 =.. [Functor2|NewArgs2].

cartesian_product_head_predicate(R1,Head1,[Var1],R2,Head2,[Var2,Var3|RestVars],OpposesList):-	
	!,cartesian_product_head_predicate(R1,Head1,[Var1],R2,Head2,[Var2],OpposesList1),
	cartesian_product_head_predicate(R1,Head1,[Var1],R2,Head2,[Var3|RestVars],OpposesList2),	
	basics:append(OpposesList1,OpposesList2,OpposesList).

cartesian_product_head_predicate(R1,Head1,[Var1|RestVar1],R2,Head2,[Var2|RestVar2],OpposesList):-	
	!,cartesian_product_head_predicate(R1,Head1,[Var1],R2,Head2,[Var2|RestVar2],OpposesList1),
	cartesian_product_head_predicate(R1,Head1,RestVar1,R2,Head2,[Var2|RestVar2],OpposesList2),	
	basics:append(OpposesList1,OpposesList2,OpposesList).

cartesian_product_head_predicate(R1,Head1,[],R2,Head2,_,[opposes(R1,NewHead1,R2,NewHead2)]):-
	!,Head1 =.. [Functor1|Args1],
	cond_var_to_ergo_var(Args1,NewArgs1,_),
	NewHead1 =.. [Functor1|NewArgs1],
	Head2 =.. [Functor2|Args2],
	cond_var_to_ergo_var(Args2,NewArgs2,_),
	NewHead2 =.. [Functor2|NewArgs2].

cartesian_product_head_predicate(R1,Head1,_,R2,Head2,[],[opposes(R1,NewHead1,R2,NewHead2)]):-
	!,Head1 =.. [Functor1|Args1],
	cond_var_to_ergo_var(Args1,NewArgs1,_),
	NewHead1 =.. [Functor1|NewArgs1],
	Head2 =.. [Functor2|Args2],
	cond_var_to_ergo_var(Args2,NewArgs2,_),
	NewHead2 =.. [Functor2|NewArgs2].	
	
add_opposes_predicate(SentenceList,DRS,R1,R2,OpposesList):-
	get_opposes_drs(SentenceList,DRS,R1,LabelledConds1,PList1),
	get_subject_var_from_plist(PList1,SubjectVars1),
	get_opposes_drs(SentenceList,DRS,R2,LabelledConds2,PList2),
	get_subject_var_from_plist(PList2,SubjectVars2),
	(LabelledConds1 = [Drs1=>Drs2], LabelledConds2 = [Drs3=>Drs4]
	->
	retractall(gl_var_ctr(_)),
	assert(gl_var_ctr(0)),
	head_predicate(R1,Head1),
	Head1 =.. [Functor1|ForallVars1],
	head_predicate(R2,Head2),
	Head2 =.. [Functor2|ForallVars2],
	find_common_subject_vars(ForallVars1,SubjectVars1,CommonVars1),
	find_common_subject_vars(ForallVars2,SubjectVars2,CommonVars2),
	cartesian_product_head_predicate(R1,Head1,CommonVars1,R2,Head2,CommonVars2,OpposesList)
	;
	true
	).
	
%	BodyPredicate1 =.. [Functor1|Args1],
%	cond_var_to_ergo_var(Args1,NewArgs1,SubjRef1),
%	Arg1 =.. [Functor1|NewArgs1],
%	body_predicate(R2,BodyPredicate2),
%	BodyPredicate2 =.. [Functor2|Args2],
%	cond_var_to_ergo_var(Args2,NewArgs2,SubjRef2),
%	Arg2 =.. [Functor2|NewArgs2],
%	OpposesList = [opposes(R1,Arg1,R2,Arg2)]
%	;
	
%add_opposes_predicate(SentenceList,DRS,R1,R2,OpposesList):-
%	get_opposes_drs(SentenceList,DRS,R1,LabelledConds1,P1-SID1/TID1),
%	get_opposes_drs(SentenceList,DRS,R2,LabelledConds2,P2-SID2/TID2),
%	P1 =.. [Func1,Ref1,Verb1,SubjRef1|_],	
%	P2 =.. [Func2,Ref2,Verb2,SubjRef2|_],	
%	(LabelledConds1 = [Drs1=>Drs2], LabelledConds2 = [Drs3=>Drs4]
%	->
%	retractall(gl_var_ctr(_)),
%	assert(gl_var_ctr(0)),
%	body_predicate(R1,BodyPredicate1),
%	BodyPredicate1 =.. [Functor1|Args1],
%	cond_var_to_ergo_var(Args1,NewArgs1,SubjRef1),
%	Arg1 =.. [Functor1|NewArgs1],
%	body_predicate(R2,BodyPredicate2),
%	BodyPredicate2 =.. [Functor2|Args2],
%	cond_var_to_ergo_var(Args2,NewArgs2,SubjRef2),
%	Arg2 =.. [Functor2|NewArgs2],
%	OpposesList = [opposes(R1,Arg1,R2,Arg2)]
%	;
%	LabelledConds1 = [_-_/_|_], LabelledConds2 = [_-_/_|_]
%	->
%	retractall(gl_var_ctr(_)),
%	assert(gl_var_ctr(0)),
%	condslist_to_predslist(LabelledConds1,ErgoConds1,_),
%	condslist_to_predslist(LabelledConds2,ErgoConds2,_),
%	cartesian_product_conds(R1,ErgoConds1,R2,ErgoConds2,OpposesList)
%	;
%	LabelledConds1 = [Drs1=>Drs2], LabelledConds2 = [_-_/_|_]
%	->
%	retractall(gl_var_ctr(_)),
%	assert(gl_var_ctr(0)),
%	body_predicate(R1,BodyPredicate1),
%	BodyPredicate1 =.. [Functor1|Args1],
%	cond_var_to_ergo_var(Args1,NewArgs1,_),
%	ErgoConds1 =.. [Functor1|NewArgs1],
%	condslist_to_predslist(LabelledConds2,ErgoConds2,_),
%	cartesian_product_conds(R1,[ErgoConds1],R2,ErgoConds2,OpposesList)
%	).
	
		
add_opposes_conds(ST,DRS,OpposesList):-
	findall(exception_list(X,Y),exception_list(X,Y),ExceptionList),
	build_opposes_list(ST,DRS,ExceptionList,OpposesList).

build_opposes_list(ST,DRS,[exception_list(ID1,ID2)|RestArgs],OpposesList):-
	add_opposes_predicate(ST,DRS,r(ID1),r(ID2),Opposes1),
	build_opposes_list(ST,DRS,RestArgs,RestOpposes),
	basics:append(Opposes1,RestOpposes,OpposesList).
	
	
build_opposes_list(_,_,[],[]).
	
%%	SubjRef1 = SubjRef2,
%%	(var(SubjRef1)
%%	->
%%	SubjRef1 = '?Y0'
%%	;
%%	true
%%	),
%%	retractall(gl_var_ctr(_)),
%%	assert(gl_var_ctr(0)),
%%	condslist_to_predslist(Conds1,ErgoConds1),
%%	condslist_to_predslist(Conds2,ErgoConds2),
%%	cartesian_product_conds(R1,ErgoConds1,R2,ErgoConds2,OpposesList).
	

condslist_to_predslist([],[],SubjVar):-!.	
condslist_to_predslist([Condition-SID/TID|RestConditions],[NewCondition|NewRestConditions],SubjVar):-
	!,Condition =.. [Functor|Arguments],
	NewCondition =.. [Functor,'\#0'|Arguments],
	condslist_to_predslist(RestConditions,NewRestConditions,SubjVar).

cond_var_to_ergo_var([],[],SubjVar):-!.
cond_var_to_ergo_var([Arg|RestArgs],[NewArg|NewRestArgs],SubjVar):-
	(Arg == SubjVar
	->
	NewArg = '?Y'
	;
	inc_var_ctr(NextID),
	assert(gl_var_ctr(NextID)),
	string:concat_atom(['?Y',NextID],NewArg)
	),
	cond_var_to_ergo_var(RestArgs,NewRestArgs,SubjVar).
	
cartesian_product_conds(R1,[Cond1],R2,[Cond2],[opposes(R1,Cond1,R2,Cond2)]):-!.

cartesian_product_conds(R1,[Cond1],R2,[Cond2,Cond3|RestConds],[opposes(R1,Cond1,R2,Cond2)|RestOpposes]):-
	!,cartesian_product_conds(R1,[Cond1],R2,[Cond3|RestConds],RestOpposes).

cartesian_product_conds(R1,[Cond1|RestConds1],R2,[Cond2|RestConds2],OpposesList):-
	!,cartesian_product_conds(R1,[Cond1],R2,[Cond2|RestConds2],OpposesList1),
	cartesian_product_conds(R1,RestConds1,R2,[Cond2|RestConds2],RestOpposesLists),
	basics:append(OpposesList1,RestOpposesLists,OpposesList).