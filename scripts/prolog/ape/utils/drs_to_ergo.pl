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
:- op(650, xfy, ~~>).           % weak implication 

:- dynamic ergo_var_counter/1.

drs_ergo(DRS,Ergo_Rules):-
	drs_ergo_init,
	drs_to_ergo(DRS,Ergo_Rules).
%
% Initialize glabal variables before translating DRS to Ergo Rules.
%
	
drs_ergo_init:-
	retractall(ergo_var_counter(_)),
	assert(ergo_var_counter(0)).
%
% drs_to_ergo(+DRS,-Ergo_Rules)
%
% translate DRS to Ergo rules
%
% @param DRS is the attempto DRS
% @param FC is the Ergo clauses(rules, and facts)
%
	
drs_to_ergo(DRS,Ergo_Rules):-
	open('/Users/tiantiangao/Documents/Research/2015-09-13/ergo/rules.ergo',write,Stream),
	write(Stream,':-use_argumentation_theory.\n'),
	drs_to_ergo_1(DRS,'sk0',Ergo_Rules,Stream),
	close(Stream).

	
%	
% drs_to_ergo_1(+DRS,+World,-Ergo_Rules,+Stream)
%
% Process the first level of DRS: 
%	1. Skolemize all existentially quantified variables in level 1
% 	2. recursively process each conditions in level 1
%
% @param DRS is the attempto DRS
% @param World is set to sk0
% @param Ergo_Rules is Ergo Rules
% @param Stream is Prolog output stream
%
	
drs_to_ergo_1(drs(Refs,Conds),World,Ergo_Rules,Stream):-
	skolemize_variables(Refs),
	cond_to_ergo_1(Conds,World,Ergo_Rules,Stream).

%
% skolemize_variables(+ListofVariables)
% Skolemize variables
%
% @param ListofVariables is a list of DRS referents
%

skolemize_variables([Quant|RestQuant]):-
	(var(Quant)
	->
	inc_ergo_var_counter(NextID),
	string:concat_atom(['sk',NextID],Var),
	assert(gl_ergo_var_counter(NextID)),Quant = Var,
	skolemize_variables(RestQuant)
	;
	skolemize_variables(RestQuant)
	).
	
skolemize_variables([]).

% cond_to_ergo_1(+Condition_List,+World,-Ergo_Rules_Atom,+Stream)
%
% translate a list of conditions in the first level of DRS into ergo facts and rules.
%
% @param Condition_List is a list of DRS conditions (simple or complex)
% @param World is a skolem constant, 'sk0'(Possible World Semantics)
% @param Ergo_Rules_Atom is a string concatenation of ergo rules
% @param Stream	is Prolog output stream

cond_to_ergo_1([Condition],World,Ergo_Rule_Atom,Stream):-
	!,cond_to_ergo(Condition,World,Ergo_Rule,[],0,0),
	write(Stream,Ergo_Rule),
	write(Stream,'.\n'),
	string:concat_atom([Ergo_Rule,'.\n'],Ergo_Rule_Atom).
	
cond_to_ergo_1([Condition1,Condition2|RestConditions],World,Ergo_Rules_Atom,Stream):-
	!,cond_to_ergo(Condition1,World,Ergo_Rule1,[],0,0),
	write(Stream,Ergo_Rule1),
	write(Stream,'.\n'),
	cond_to_ergo_1([Condition2|RestConditions],World,Rest_Ergo_Rules,Stream),
	string:concat_atom([Ergo_Rule1,Rest_Ergo_Rules],'.\n',Ergo_Rules_Atom).


% drs_to_ergo(+DRS,+World,-Ergo_Clause,Dep_Var_List,+Flag,+Level)
%
% translate nested DRS to Ergo clauses
%
% @param DRS is a nested DRS
% @param World is a skolem constant, 'sk0'(Possible World Semantics)
% @param Ergo_Clause is the translated Ergo clause
% @param Dep_Var_List keeps track of all forall variables that encloses the current DRS
% @param Flag indicates whethere the nested DRS appear in rule body or NOT
% @param Level represents the level of the DRS
		
drs_to_ergo(drs([],[Condition]),World,Ergo_Clause,Dep_Var_List,Flag,_) :-
	!,
  	cond_to_ergo(Condition,World, Ergo_Clause,Dep_Var_List,Flag,_).

drs_to_ergo(drs([],[Condition1,Condition2|Conditions]),World,
												Ergo_Clause,Dep_Var_List,Flag,_):-!,
	cond_to_ergo(Condition1, World, Ergo_Clause1,Dep_Var_List,Flag,_),
  	cond_to_ergo([Condition2|Conditions], World, Ergo_Clause2,Dep_Var_List,Flag,_),
  	string:concat_atom([Ergo_Clause1,Ergo_Clause2],',',Ergo_Clause).

drs_to_ergo(drs([X|Referents],Conditions),World,Ergo_Clause,Dep_Var_List,Flag,_):-
  	!,
  	assign_vars([X|Referents],Exists_Quant,_),
  	cond_to_ergo(Conditions, World, Ergo_Clause1,Dep_Var_List,Flag,_),
  	((Exists_Quant \== '')
  	->
  	string:concat_atom(['exists(',Exists_Quant,')^(',Ergo_Clause1,')'],Ergo_clause)
  	;
  	string:concat_atom(['(',Ergo_Clause1,')'],Ergo_Clause)
  	).
 
%
% assign_vars(+Referents,-Ergo_Vars_Atom,-Ergo_Vars_List)
%
% Assign variables to DRS referents, like ?X{i} (i=1,2,...)
% 
% @param Referents refer to DRS referents
% @param Ergo_Vars_Atom is Ergo variables in string
% @param Ergo_Vars_List is a list of Ergo variables in string
%

assign_vars([Ref|RestRefs],Ergo_Vars_Atom,Ergo_Vars_List):-
	(var(Ref)
	->
	inc_ergo_var_counter(NextID),
	assert(ergo_var_counter(NextID)),
	string:concat_atom(['?X',NextID],Ergo_Var),
	Ref = Ergo_Var,
	assign_vars(RestRefs,Rest_Ergo_Vars_Atom,Rest_Ergo_Vars_List),
	string:concat_atom([Ergo_Var,Rest_Ergo_Vars_Atom],',',Ergo_Vars_Atom),
	Ergo_Vars_List = [Ergo_Var|Rest_Ergo_Vars_List]
	;
	assign_vars(RestRefs,Ergo_Vars_Atom,Ergo_Vars_List)
	).
	
assign_vars([],'',[]).

%
% cond_to_ergo(+Condition,+World,-Ergo_Clause,+Dep_Var_List,+Flag,+Level)	
%
% Transform a (nested) DRS conition into an Ergo clause or Ergo rule
%
% @param Condition is a complex DRS condition, e.g., DRS1 => DRS2, -DRS
% @param World is sk0 representing possible world semantics
% @param DepVarList is a Prolog list of Ergo vars that enclose the current condition
% @param Flag indicates whether Condition is from rule body or NOT
% @param Level = 0 first level of DRS; Level \= 0 deeper levels
%

cond_to_ergo(-Drs,World,NEG_Ergo_Clause,Dep_Var_List,Flag,_):-
	!,drs_to_ergo(Drs,World,Ergo_Clause,Dep_Var_List,Flag,_),
	(Flag == 1
	->
	string:concat_atom(['(','\\naf ',Ergo_Clause,')'],NEG_Ergo_Clause)
	;
	string:concat_atom(['(','\\neg ',Ergo_Clause,')'],NEG_Ergo_Clause)
	).


cond_to_ergo(drs(Refs1,Conds1) => drs(Refs2,Conds2),
									World,Ergo_Rule,Dep_Var_List,Flag,Level):-!,
	(Level == 0
	->
	assign_vars(Refs1,Forall_Quant,Forall_Quant_List),
	basics:append(Dep_Var_List,Forall_Quant_List,Dep_Var_List2),
	cond_to_ergo(Conds1,World,Ergo_Body,Dep_Var_List2,1,_),
	assign_vars(Refs2,Exists_Quant,Exists_Quant_List),
	cond_to_ergo(Conds2,World,Ergo_Head,Dep_Var_List2,0,_),
	(Exists_Quant == ''
	->
	string:concat_atom([Ergo_Head,':-',Ergo_Body],Ergo_Rule)
	;
	string:concat_atom(['exists(',Exists_Quant,')^(',Ergo_Head,')'],Ergo_Head2),
	string:concat_atom([Ergo_Head2,':-',Ergo_Body],Ergo_Rule)
	)
	;
	drs_cnf(drs([],[-drs([],[drs(Refs1,Conds1) => drs(Refs2,Conds2)])]),CNF_List),
	cnf_list_atom(CNF_List,Ergo_Rule)
	).

cnf_list_atom(CNF_List,CNF_List_Atom):-
	cnf_list_to_atom(CNF_List,CNF_List_Atom2),
	string:concat_atom(['%resolve([',CNF_List_Atom2,'])'],CNF_List_Atom).

cnf_list_to_atom([],'').
cnf_list_to_atom([X|Rest],CNF_List_Atom):-
	string:concat_atom(X,',',A1),
	string:concat_atom(['[',A1,']'],A2),
	cnf_list_to_atom(Rest,A3),
	string:concat_atom([A2,A3],',',CNF_List_Atom).

cond_to_ergo(RuleLabel(drs(Refs1,Conds1) => drs(Refs2,Conds2)),
											World,Ergo_Rule,Dep_Var_List,Flag,_):-
	!,assign_vars(Refs1,Forall_Quant,Forall_Quant_List),
	basics:append(Dep_Var_List,Forall_Quant_List,Dep_Var_List2),
	cond_to_ergo(Conds1,World,Ergo_Body,Dep_Var_List2,1,_),
	Rule_Label = r(ID),
	string:term_to_atom(Rule_Label,Rule_Label_Atom),
	string:term_to_atom(ID,ID_Atom),
	assign_vars(Refs2,Exists_Quant,Exists_Quant_List),
	cond_to_ergo(Conds2,World,Ergo_Head,Dep_Var_List2,0,_),
	(Exists_Quant == ''
	->
    Ergo_Head1 = Ergo_Head
	;
	string:concat_atom(['exists(',Exists_Quant,')^(',Ergo_Head,')'],Ergo_Head1)
	),
	(Forall_Quant == ''
	->
	string:concat_atom(['@{',Rule_Label_Atom,'} ','head',ID_Atom,':-',Ergo_Body],
						Ergo_Rule1),
	string:concat_atom([Ergo_Head1,':-','head',ID_Atom],Ergo_Rule2),
	string:concat_atom([Ergo_Rule1,'.\n',Ergo_Rule2],Ergo_Rule)
	;
	Forall_Quant \== ''
	->
	string:concat_atom(['@{',Rule_Label_Atom,'} ','head',IDAtom,'(',Forall_Quant,')',
						':-',ergo_Body],Ergo_Rule1),
	string:concat_atom([Ergo_Head,':-','head',IDAtom,'(',Forall_Quant,')'],Ergo_Rule2),
	string:concat_atom([Ergo_Rule1,'.\n',Ergo_Rule2],Ergo_Rule)
	).
	
cond_to_ergo([Condition],World,Ergo_Clause,Dep_Var_List,Flag,_):-
	!,cond_to_ergo(Condition,World,Ergo_Clause,Dep_Var_List,Flag,_).
	
cond_to_ergo([Condition1,Condition2|RestConditions],World,Ergo_Clause,
				Dep_Var_List,Flag,_):-
	!,cond_to_ergo(Condition1,World,Ergo_Clause1,Dep_Var_List,Flag,_),
	cond_to_ergo([Condition2|RestConditions],World,Rest_Ergo_Clause,Dep_Var_List,Flag,_),
	string:concat_atom([Ergo_Clause1,Rest_Ergo_Clause],',',Ergo_Clause).

cond_to_ergo(BasicCondition - Index, World, Ergo_Predicate,_,_,_):-
	!,BasicCondition =.. [Functor|Arguments],
	reformulate_int_or_string(Arguments,NewArguments),
    NewBasicCondition =..[Functor|[World|NewArguments]],
    string:term_to_atom(NewBasicCondition, NewBasicConditionAtom),
	string:term_to_atom(Index, IndexAtom),
	string:concat_atom([NewBasicConditionAtom],Ergo_Predicate).	

cond_to_ergo(RuleLabel(BasicCondition - Index), World, Ergo_Predicate,_,_,_):-
	!,BasicCondition =.. [Functor|Arguments],
	reformulate_int_or_string(Arguments,NewArguments),
    NewBasicCondition =..[Functor|[World|NewArguments]],
    string:term_to_atom(NewBasicCondition, NewBasicConditionAtom),
	string:term_to_atom(Index, IndexAtom),
	string:term_to_atom(RuleLabel,RuleLabelAtom),
	string:concat_atom(['(@{',RuleLabelAtom,'} ',NewBasicConditionAtom,')'],Ergo_Predicate).	

reformulate_int_or_string([],[]):-!.	
reformulate_int_or_string([Arg|RestArgs],NewArgs):-!,
	(Arg = int(X)
	->
	NewArgs = [X|RestNewArgs]
	;
	Arg = string(X)
	->
	NewArgs = [X|RestNewArgs]
	;
	Arg = named(X)
	->
	NewArgs = [X|RestNewArgs]
	;
	(Arg =.. [Var|DepVars])
	->
	reformulate_int_or_string(DepVars,NewDepVars),
	NewArg =.. [Var|NewDepVars],
	NewArgs = [NewArg|RestNewArgs]
	;
	NewArgs = [Arg|RestNewArgs]
	),reformulate_int_or_string(RestArgs,RestNewArgs).
		

inc_ergo_var_counter(NextID):-
	findall(ID,ergo_var_counter(ID),IDList),
	max_list(IDList,CurID), NextID is CurID + 1.
	