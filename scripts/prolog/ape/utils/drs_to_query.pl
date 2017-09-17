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

drs_query(drs([],[question(DRS)]),ErgoQuery):-
	retractall(gl_var_ctr(_)),
	assert(gl_var_ctr(0)),
	drs_to_query(DRS,'\#0',Query),
	string:concat_atom([Query,'.\n'],ErgoQuery).
	
drs_to_query(drs([],[Condition]),World,Ergo_Predicate) :-
  !,
  cond_to_ergo(Condition,World, Ergo_Predicate).

drs_to_query(drs([],[Condition1,Condition2|Conditions]), World, Ergo_Predicates) :-
  !,
  cond_to_ergo(Condition1, World, Ergo_Predicate1),
  cond_to_ergo([Condition2|Conditions], World, Ergo_Predicate2),
  string:concat_atom([Ergo_Predicate1,Ergo_Predicate2],',',Ergo_Predicates).

drs_to_query(drs([X|Referents],Conditions), World, Ergo_Query) :-
  !,
  process_quant([X|Referents]),
  cond_to_ergo(Conditions, World, Ergo_Query).
  
process_quant([]):-!.	  
process_quant([Quant|RestQuant]):-
	inc_var_ctr(NextID),
	string:concat_atom(['?','X',NextID],Var),
	assert(gl_var_ctr(NextID)),Quant = Var,
	process_quant(RestQuant).
	
cond_to_ergo(drs(Ref1,Conds1) v Drs2,World,Ergo_Query):-
	!,process_quant(Ref1),
	cond_to_ergo(Conds1,World,Ergo_Query1),
	drs_to_query(Drs2,World,Ergo_Query2),
	string:concat_atom(['((',Ergo_Query1,')',' \or (',Ergo_Query2,'))'],Ergo_Query).
	
%% what it doesn't handle
%% 1. if ... then statements
%% 2. must 
%% 3. negation [Who does not have a cat?]
%% 4. naf
%% 5. [bug] Mary has a cat or a dog. Who has a cat or has a dog?

cond_to_ergo(can(DRS),World1,Ergo_Query):-
	!,process_quant([World2]),
	drs_to_query(DRS,World2,Ergo_Query1),
	string:concat_atom(['accessibility_relation(',World1,',',World2,'),',Ergo_Query1],Ergo_Query).
	
cond_to_ergo(World2:DRS,World1,Ergo_Query):-
	!,drs_to_query(DRS,World2,Ergo_Query1),
	string:concat_atom(['accessibility_relation(',World1,',',World2,'),',Ergo_Query1],Ergo_Query).

cond_to_ergo([Condition],World,Ergo_Query):-
	!,cond_to_ergo(Condition,World,Ergo_Query).

cond_to_ergo([Condition1,Condition2|RestConditions],World,Ergo_Query):-
	Condition1 = Predicate1-Index,
	Predicate1 =.. [Functor|Arguments],
	Functor == query,!,
	cond_to_ergo([Condition2|RestConditions],World,Ergo_Query).
	
cond_to_ergo([Condition1,Condition2|RestConditions],World,Ergo_Query):-
	!,cond_to_ergo(Condition1,World,Query_Predicate),
	cond_to_ergo([Condition2|RestConditions],World,Rest_Query_Predicates),
	(Rest_Query_Predicates == ''
	->
	Ergo_Query = Query_Predicate
	;
	string:concat_atom([Query_Predicate,Rest_Query_Predicates],',',Ergo_Query)
	).

cond_to_ergo(BasicCondition - Index, World, Ergo_Query):-
	!,BasicCondition =.. [Functor|Arguments],
	(Functor == query
	->
	Ergo_Query = ''
	;
    NewBasicCondition =..[Functor|[World|Arguments]],
    string:term_to_atom(NewBasicCondition, Ergo_Query)
	).