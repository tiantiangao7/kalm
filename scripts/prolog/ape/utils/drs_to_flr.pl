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

:- dynamic gl_var_ctr/1.
:- dynamic def_rule_head/2.
:- dynamic head_predicate/2.
:- dynamic aux_axiom/1.
:- dynamic gl_formula_id_ctr/1.
:- dynamic gl_syntax_tree/1.

drs_flr(DRS,SyntaxTree,FC):-
	drs_flr_init,
	assert(gl_syntax_tree(SyntaxTree)),
	%open('d:\\rules.ergo',write,Stream),
	%write(Stream,':-use_argumentation_theory.\n'),
	%drs_to_flr_1(DRS,'sk0',FC,Stream),
	%%def_rule_head_to_atom(DefRuleHeadsAtom),
	%%(DefRuleHeadsAtom == ''
	%%->
	%string:concat_atom([FC,'\n'],ErgoProg),
	%close(Stream),
	%%;
	%%string:concat_atom(['(',FC,',(',DefRuleHeadsAtom,')).\n'],ErgoProg)
	%%),
	output_ergo_to_file(DRS,FC),
	output_exception_info,
	output_aux_axiom_info.

%
% Initialize glabal variables and auxilliary axioms before translating DRS to Ergo Rules.
%
	
drs_flr_init:-
	retractall(gl_var_ctr(_)),
	retractall(gl_formula_id_ctr(_)),
	retractall(gl_syntax_tree(_)),
	retractall(def_rule_head(_,_)),
	retractall(head_predicate(_,_)),
	retractall(aux_axiom(_)),
	assert(gl_var_ctr(0)),
	assert(gl_formula_id_ctr(0)),
	assert(aux_axiom('object(?W,?X,something,dom,na,na,na):-object(?W,?X,?,?,?,?,?)')),
	assert(aux_axiom('true(?X):-fact(?X)')),
	assert(aux_axiom('true(?X):-?X,!!')).

%
% output_ergo_to_file(+DRS,-FC)
%
% translate DRS to Ergo rules
%
% @param DRS is the attempto DRS
% @param FC is the Ergo clauses(rules, and facts)
%
	
output_ergo_to_file(DRS,FC):-
	open('/Users/tiantiangao/Documents/Research/2015-09-13/ergo/rules.ergo',write,Stream),
	write(Stream,':-use_argumentation_theory.\n'),
	drs_to_flr_1(DRS,'sk0',FC,Stream),
	close(Stream).

%
% Write auxilliary axioms into a file
%	
	
output_aux_axiom_info:-
	findall(aux_axiom(X),aux_axiom(X),AuxAxiomList),
	open('/Users/tiantiangao/Documents/Research/2015-09-13/ergo/rules.ergo',append,Stream),
	write_aux_axiom(Stream,AuxAxiomList),
	close(Stream).

%
% write_aux_axiom(+Stream,+ListofAuxilliaryAxioms)
%
% @param Stream is the Prolog output stream
% @param ListofAuxilliaryAxioms is a list of predictes of form aux_axiom(...) where the argument is a string containing the axiom
%

write_aux_axiom(Stream,[]):-!.
write_aux_axiom(Stream,[aux_axiom(X)|RestAuxAxioms]):-
	!,write(Stream,X),
	write(Stream,'.\n'),
	write_aux_axiom(Stream,RestAuxAxioms).
	
%
% output overriding relations
%
	
output_exception_info:-
	findall(exception_list(X,Y),exception_list(X,Y),ExList),
	open('/Users/tiantiangao/Documents/Research/2015-09-13/ergo/rules.ergo',append,Stream),
	write_overrides_predicate(Stream,ExList),
	close(Stream).

%
% write_overrides_predicate(+Stream,+ListofOverridesRelations)
%
% @param Stream is the Prolog output stream
% @param ListofOverrideRelations is a list of \overrides predicates
%

write_overrides_predicate(Stream,[]):-!.
write_overrides_predicate(Stream,[ExceptionCondition|RestExceptionCondition]):-
	!,ExceptionCondition =.. [Functor,Arg1,Arg2],
	string:concat_atom(['\overrides(r(',Arg1,'),r(',Arg2,')).\n'],OverridesPredicate),
	write(Stream,OverridesPredicate),
	write_overrides_predicate(Stream,RestExceptionCondition).
	
%
% This part of code needs to be commented out
%

def_rule_head_to_atom(DefRuleHeadsAtom):-
	findall(def_rule_head(X,Y),def_rule_head(X,Y),Def_Head_List),
	write_def_rule_head(Def_Head_List,DefRuleHeadsAtom).

write_def_rule_head([],''):-!.
write_def_rule_head([RuleHead|RestRuleHead],RuleHeadList):-
	!,string:term_to_atom(RuleHead,RuleHeadAtom),
	write_def_rule_head(RestRuleHead,RestRuleHeadAtom),
	(RestRuleHeadAtom == ''
	->
	RuleHeadList = RuleHeadAtom
	;
	string:concat_atom([RuleHeadAtom,',',RestRuleHeadAtom],RuleHeadList)
	).
	
%	
% drs_to_flr_1(+DRS,+World,-FLR_Rules,+Stream)
%
% Process the first level of DRS: 
%	1. Skolemize all existentially quantified variables in level 1
% 	2. recursively process each conditions in level 1
%
% @param DRS is the attempto DRS
% @param World is set to sk0
% @param FLR_Rules is Ergo Rules
% @param Stream is Prolog output stream
%
	
drs_to_flr_1(drs(Refs,Conds),World,FLR_Rules,Stream):-
	replace_skolem(Refs),
	cond_to_flr_1(Conds,World,FLR_Rules,Stream).

%
% replace_skolem(+ListofVariables)
% Skolemize variables
%
% @param ListofVariables is a list of DRS referents
%

replace_skolem([Quant|RestQuant]):-
	(var(Quant)
	->
	inc_var_ctr(NextID),
	string:concat_atom(['sk',NextID],Var),
	assert(gl_var_ctr(NextID)),Quant = Var,
	replace_skolem(RestQuant)
	;
	replace_skolem(RestQuant)
	).
	
replace_skolem([]).
		
drs_to_flr(drs([],[Condition]),World,FLR_Predicate,DepVarList,Flag,_) :-
  !,
  cond_to_flr(Condition,World, FLR_Predicate,DepVarList,Flag,_).

drs_to_flr(drs([],[Condition1,Condition2|Conditions]), World, FLR_Predicates,DepVarList,Flag,_):-
  !,
  cond_to_flr(Condition1, World, FLR_Predicate1,DepVarList,Flag,_),
  cond_to_flr([Condition2|Conditions], World, FLR_Predicate2,DepVarList,Flag,_),
  string:concat_atom([FLR_Predicate1,FLR_Predicate2],',',FLR_Predicates).

drs_to_flr(drs([X|Referents],Conditions), World, FLR_Rule,DepVarList,Flag,_) :-
  !,
  process_refs([X|Referents],ExistsQuant,_),
  cond_to_flr(Conditions, World, FLR_Predicates,DepVarList,Flag,_),
  ((ExistsQuant \== '')
  ->
  string:concat_atom(['exists(',ExistsQuant,')^(',FLR_Predicates,')'],FLR_Rule)
  ;
  string:concat_atom(['(',FLR_Predicates,')'],FLR_Rule)
  ).
 
%
% The following code is not currently used
%
/* 
assign_skolem_var(Ref,[Condition-Index|RestConditions],DepVarList,ExistsVar):-
	(
	(Condition =.. [object,Ref1|_],Ref1 == Ref)
	->
	Ref =.. [ExistsVar|DepVarList]
	;
	(Condition =.. [predicate,Ref1,Verb|RestObjects],Ref1 == Ref)
	->
  	Ref =.. [ExistsVar|RestObjects]
	;
	(Condition =.. [has_part,Ref1|_],Ref1 == Ref)
	->
	Ref =.. [ExistsVar|DepVarList]
	;
	assign_skolem_var(Ref,RestConditions,DepVarList,ExistsVar)
	).
*/
%
% skolemize_ergo_vars(+Referents,+Conditions,+DepVarList,-ExistsVarListAtom,-ExistsVarList,+Flag)
%
% Skolemize referents of DRS2 in the form of DRS1 => DRS2
%
% @param Referents are DRS2 referents
% @param Conditions are DRS2 conditions. They are not currently used.
% @param DepVarList are all the forall quantified vars that enclose DRS2
% @param ExistsVarListAtom is a string of skolemized vars
% @param ExistsVarList is a Prolog list of skolimized vars in string
% @param Flag = 1, Assign ?Xi; Flag = 0, Assign ski
%
	
skolemize_ergo_vars([Ref|RestRefs],Conditions,DepVarList,ExistsVarListAtom,ExistsVarList,Flag):-
	(var(Ref)
	->
	inc_var_ctr(NextID),
	assert(gl_var_ctr(NextID)),
	(Flag == 1
	->
	string:concat_atom(['?X',NextID],ExistsVar),
	Ref = ExistsVar
	;
	string:concat_atom(['sk',NextID],ExistsVar),
	Ref = ExistsVar
	),
	skolemize_ergo_vars(RestRefs,Conditions,DepVarList,RestExistsVarListAtom,RestExistsVarList,Flag),
	string:concat_atom([ExistsVar,RestExistsVarListAtom],',',ExistsVarListAtom),
	ExistsVarList = [ExistsVar|RestExistsVarList]
	;
	skolemize_ergo_vars(RestRefs,Conditions,DepVarList,ExistsVarListAtom,ExistsVarList,Flag)
	).
  
skolemize_ergo_vars([],_,_,'',[],_).

%
% process_refs(+Referents,-ErgoVars,-PrologVars)
%
% Translate a list of DRS referents to Ergo variables of form ?Xi (i=1,2,...)
% 
% @param Referents refer to DRS referents
% @param ErgoVars is Ergo variables in string
% @param PrologVars is a list of Ergo variables in string
%

process_refs([Ref|RestRefs],ErgoVars,PrologVars):-
	(var(Ref)
	->
	inc_var_ctr(NextID),
	assert(gl_var_ctr(NextID)),
	string:concat_atom(['?','X',NextID],ErgoVar),
	string:concat_atom(['?X',NextID],PrologVar),
	Ref = ErgoVar,
	process_refs(RestRefs,RestErgoVars,RestPrologVars),
	string:concat_atom([ErgoVar,RestErgoVars],',',ErgoVars),
	PrologVars = [PrologVar|RestPrologVars]
	;
	process_refs(RestRefs,ErgoVars,PrologVars)
	).
	
process_refs([],'',[]).

% this will never happen
%cond_to_flr(drs(Refs,Conds),FLR_Predicates):-
%	!,drs_to_flr(drs(Refs,Conds),FLR_Predicates).

cond_to_flr(-Drs,World,NEG_FLR_Rule,DepVarList,Flag,_):-
	!,drs_to_flr(Drs,World,FLR_Rule,DepVarList,Flag,_),
	(Flag == 1
	->
	string:concat_atom(['(','\\naf ',FLR_Rule,')'],NEG_FLR_Rule)
	;
	string:concat_atom(['(','\\neg ',FLR_Rule,')'],NEG_FLR_Rule)
	).

cond_to_flr(RuleLabel(-Drs),World,NEG_FLR_Rule,DepVarList,Flag,_):-
	!,drs_to_flr(Drs,World,FLR_Rule,DepVarList,Flag,_),
	string:term_to_atom(RuleLabel,RuleLabelAtom),
	string:concat_atom(['@{',RuleLabelAtom,'} ','\\neg ',FLR_Rule],NEG_FLR_Rule).

cond_to_flr(~Drs,World,NEG_FLR_Rule,DepVarList,Flag,_):-
	!,drs_to_flr(Drs,World,FLR_Rule,DepVarList,Flag,_),
	string:concat_atom(['(','\\naf ',FLR_Rule,')'],NEG_FLR_Rule).

cond_to_flr(RuleLabel(~Drs),World,NEG_FLR_Rule,DepVarList,Flag,_):-
	!,drs_to_flr(Drs,World,FLR_Rule,DepVarList,Flag,_),
	string:term_to_atom(RuleLabel,RuleLabelAtom),
	string:concat_atom(['@{',RuleLabelAtom,'} ','\\naf ',FLR_Rule],NEG_FLR_Rule).
	
%cond_to_flr(drs(Ref1,Conds1) v Drs2,World,FLR_Rule):-
%	!,process_refs(Ref1,ExistsQuant,_),
%	cond_to_flr(Conds1,World,FLR_Rule1),
%	drs_to_flr(Drs2,World,FLR_Rule2),
%	(ExistsQuant == ''
%	->
%	string:concat_atom(['((',FLR_Rule1,')',' \or (',FLR_Rule2,'))'],FLR_Rule)
%	;
%	string:concat_atom(['exists(',ExistsQuant,')^((',FLR_Rule1,')',' \or (',FLR_Rule2,'))'],FLR_Rule)
%	).

%cond_to_flr(RuleLabel(drs(Ref1,Conds1) v Drs2),World,FLR_Rule):-
%	!,process_refs(Ref1,ExistsQuant,_),
%	cond_to_flr(Conds1,World,FLR_Rule1),
%	drs_to_flr(Drs2,World,FLR_Rule2),
%	string:term_to_atom(RuleLabel,RuleLabelAtom),
%	(ExistsQuant == ''
%	->
%	string:concat_atom(['@{',RuleLabelAtom,'} ','(',FLR_Rule1,')',' \or (',FLR_Rule2,')'],FLR_Rule)
%	;
%	string:concat_atom(['@{',RuleLabelAtom,'} ','exists(',ExistsQuant,')^((',FLR_Rule1,')',' \or (',FLR_Rule2,'))'],FLR_Rule)
%	).

/*
%% extract antecedent conds
%% exit: a list of simple conditions (or \neg)
%% need to define the gramma for a basic sentence. 
%% a basic sentence shouldn't contain implication
	
extract_antecedent([drs(Refs1,Conds1) => drs(Refs2,Conds2)],World,DepVarList,AntecedentList,Consequent,LastDepVarList):-	
	!,process_refs(Refs1,ForallQuant,ForallVarList),
	basics:append(DepVarList,ForallVarList,NewDepVarList),
	cond_to_flr(Conds1,World,Antecedent1,NewDepVarList,1,_),
	((Refs2 = [],Conds2 = [Drs1 => Drs2])
	->
	extract_antecedent(Conds2,World,NewDepVarList,RestAntecedentList,Consequent,LastDepVarList),
	AntecedentList = [Antecedent1|RestAntecedentList]
	;
	skolemize_ergo_vars(Refs2,Conds2,NewDepVarList,ExistsQuant,_,0),
	cond_to_flr(Conds2,World,Consequent,NewDepVarList,1,_),
	AntecedentList = [Antecedent1],
	LastDepVarList = NewDepVarList
	).
*/
	
%
% cond_to_flr(+Condition,+World,-FLR_Rule,+DepVarList,+Flag,+Level)	
%
% Transform a (nested) DRS conition into an Ergo clause or Ergo rule
%
% @param Condition is a DRS condition in forms of DRS1 => DRS2, DRS1 ~~> DRS2, RuleLable(DRS1 => DRS2), etc..
% @param World is sk0 representing possible world semantics
% @param DepVarList is a Prolog list of Ergo vars that enclose the current condition
% @param Flag = 1 indicates Condition is from rule body; Flag = 0 indicates Condition is from rule head
% @param Level = 0 first level of DRS; Level \= 0 deeper levels
%
	
cond_to_flr(drs(Refs1,Conds1) ~~> drs(Refs2,Conds2),World,FLR_Rule,DepVarList,Flag,Level):-!,
	process_refs(Refs1,ForallQuant,ForallVarList),
	basics:append(DepVarList,ForallVarList,NewDepVarList),
	cond_to_flr(Conds1,World,FLR_Body,NewDepVarList,1,_),
	skolemize_ergo_vars(Refs2,Conds2,NewDepVarList,ExistsQuant,_,1),
	cond_to_flr(Conds2,World,FLR_Head,NewDepVarList,1,_),
	(ForallQuant == '',ExistsQuant == ''
	->
	string:concat_atom(['(',FLR_Body,'~~>',FLR_Head,')'],FLR_Rule)	
	;
	ForallQuant == '',ExistsQuant \== ''
	->
	string:concat_atom(['(',FLR_Body,'~~> exists(',ExistsQuant,')^(',FLR_Head,'))'],FLR_Rule)
	;
	ForallQuant \== '',ExistsQuant == ''
	->
	string:concat_atom(['(forall(',ForallQuant,')^(',FLR_Body,'~~>',FLR_Head,'))'],FLR_Rule)
	;
	ForallQuant \== '',ExistsQuant \== ''	
	->
	string:concat_atom(['(forall(',ForallQuant,')^(',FLR_Body,'~~> exists(',ExistsQuant,')^(',FLR_Head,')))'],FLR_Rule)
	).	

cond_to_flr(drs(Refs1,Conds1) => drs(Refs2,Conds2),World,FLR_Rule,DepVarList,Flag,Level):-!,
	(Level == 0
	->
	process_refs(Refs1,ForallQuant,ForallVarList),
	basics:append(DepVarList,ForallVarList,NewDepVarList),
	cond_to_flr(Conds1,World,FLR_Body,NewDepVarList,1,_),
	skolemize_ergo_vars(Refs2,Conds2,NewDepVarList,ExistsQuant,_,1),
	cond_to_flr(Conds2,World,FLR_Head0,NewDepVarList,0,_),
	(ExistsQuant == ''
	->
	string:concat_atom([FLR_Head0],FLR_Head),
	string:concat_atom([FLR_Head,':-',FLR_Body],FLR_Rule)
	;
	string:concat_atom(['exists(',ExistsQuant,')^(',FLR_Head0,')'],FLR_Head),
	string:concat_atom([FLR_Head,':-',FLR_Body],FLR_Rule)
	)
	;
	process_refs(Refs1,ForallQuant,ForallVarList),
	basics:append(DepVarList,ForallVarList,NewDepVarList),
	cond_to_flr(Conds1,World,FLR_Body,NewDepVarList,1,_),
	skolemize_ergo_vars(Refs2,Conds2,NewDepVarList,ExistsQuant,_,1),
	cond_to_flr(Conds2,World,FLR_Head,NewDepVarList,0,_),
	(ForallQuant == '',ExistsQuant == ''
	->
	string:concat_atom(['(',FLR_Body,'==>',FLR_Head,')'],FLR_Rule)	
	;
	ForallQuant == '',ExistsQuant \== ''
	->
	string:concat_atom(['(',FLR_Body,'==> exists(',ExistsQuant,')^(',FLR_Head,'))'],FLR_Rule)
	;
	ForallQuant \== '',ExistsQuant == ''
	->
	string:concat_atom(['(forall(',ForallQuant,')^(',FLR_Body,'==>',FLR_Head,'))'],FLR_Rule)
	;
	ForallQuant \== '',ExistsQuant \== ''	
	->
	string:concat_atom(['(forall(',ForallQuant,')^(',FLR_Body,'==> exists(',ExistsQuant,')^(',FLR_Head,')))'],FLR_Rule)
	)).

cond_to_flr(RuleLabel(drs(Refs1,Conds1) => drs(Refs2,Conds2)),World,FLR_Rule,DepVarList,Flag,_):-
	!,
	process_refs(Refs1,ForallQuant,ForallVarList),
	basics:append(DepVarList,ForallVarList,NewDepVarList),
	cond_to_flr(Conds1,World,FLR_Body,NewDepVarList,1,_),
	RuleLabel = r(ID),
	string:term_to_atom(RuleLabel,RuleLabelAtom),
	string:term_to_atom(ID,IDAtom),
	skolemize_ergo_vars(Refs2,Conds2,NewDepVarList,ExistsQuant,_,1),
	cond_to_flr(Conds2,World,FLR_Head0,NewDepVarList,0,_),
	(ExistsQuant == ''
	->
	string:concat_atom([FLR_Head0],FLR_Head)
	;
	string:concat_atom(['exists(',ExistsQuant,')^(',FLR_Head0,')'],FLR_Head)
	),
	(ForallQuant == ''
	->
	string:concat_atom(['@{',RuleLabelAtom,'} ','head',IDAtom,':-',FLR_Body],FLR_Rule1),
	string:concat_atom([FLR_Head,':-','head',IDAtom],FLR_Rule2),
	string:concat_atom([FLR_Rule1,'.\n',FLR_Rule2],FLR_Rule)
	;
	ForallQuant \== ''
	->
	string:concat_atom(['@{',RuleLabelAtom,'} ','head',IDAtom,'(',ForallQuant,')',':-',FLR_Body],FLR_Rule1),
	string:concat_atom([FLR_Head,':-','head',IDAtom,'(',ForallQuant,')'],FLR_Rule2),
	string:concat_atom([FLR_Rule1,'.\n',FLR_Rule2],FLR_Rule)
	).
	
%% The following code is commented out
%% BEGIN COMMENTING

/*
	
%% create a fact: fact(${Body==>Head}).
%% do we need to specify forall or not? probably yes. use LastDepVarList
%% 
	
cond_to_flr(drs(Refs1,Conds1) => drs(Refs2,Conds2),World,FLR_Rule,DepVarList,Flag,Level):-!,
	(Level == 0
	->
	copy_term(drs(Refs1,Conds1) => drs(Refs2,Conds2),NewImpl),
	process_refs(Refs1,ForallQuant,ForallVarList),
	basics:append(DepVarList,ForallVarList,NewDepVarList),
	cond_to_flr(Conds1,World,FLR_Body,NewDepVarList,1,_),
	((Refs2 = [], Conds2 = [SubDrs1 => SubDrs2])
	->
	extract_antecedent(Conds2,World,NewDepVarList,AntecedentList,Consequent,_),
	string:concat_atom([FLR_Body|AntecedentList],',',New_FLR_Body),
	string:concat_atom([Consequent,':-',New_FLR_Body],FLR_Rule1),
	%%string:concat_atom(['fact(${',New_FLR_Body,'==>',Consequent,'})'],FLR_Rule2)
	cond_to_flr(NewImpl,World,FLR_Rule2,DepVarList,Flag,-1)
	;
	skolemize_ergo_vars(Refs2,Conds2,NewDepVarList,ExistsQuant,_,0),
	cond_to_flr(Conds2,World,FLR_Head,NewDepVarList,0,_),
	string:concat_atom([FLR_Head,':-',FLR_Body],FLR_Rule1),
	%%string:concat_atom(['fact(${',FLR_Body,'==>',FLR_Head,'})'],FLR_Rule2)
	cond_to_flr(NewImpl,World,FLR_Rule2,DepVarList,Flag,-1)
	%%string:concat_atom(['fact(${',FLR_Rule_NewImpl,'})'],FLR_Rule2)
	),
	string:concat_atom([FLR_Rule1,FLR_Rule2],'.\n',FLR_Rule)
	;
	process_refs(Refs1,ForallQuant,ForallVarList),
	basics:append(DepVarList,ForallVarList,NewDepVarList),
	cond_to_flr(Conds1,World,FLR_Body,NewDepVarList,1,_),
	skolemize_ergo_vars(Refs2,Conds2,NewDepVarList,ExistsQuant,_,1),
	cond_to_flr(Conds2,World,FLR_Head,NewDepVarList,1,_),
	(ForallQuant == '',ExistsQuant == ''
	->
	string:concat_atom(['(',FLR_Body,'==>',FLR_Head,')'],FLR_Rule1)	
	;
	ForallQuant == '',ExistsQuant \== ''
	->
	string:concat_atom(['(',FLR_Body,'==> exists(',ExistsQuant,')^(',FLR_Head,'))'],FLR_Rule1)
	;
	ForallQuant \== '',ExistsQuant == ''
	->
	string:concat_atom(['(forall(',ForallQuant,')^(',FLR_Body,'==>',FLR_Head,'))'],FLR_Rule1)
	;
	ForallQuant \== '',ExistsQuant \== ''	
	->
	string:concat_atom(['(forall(',ForallQuant,')^(',FLR_Body,'==> exists(',ExistsQuant,')^(',FLR_Head,')))'],FLR_Rule1)
	),
	(Level == -1
	->
	string:concat_atom(['fact(${',FLR_Rule1,'})'],FLR_Rule)
	;
	string:concat_atom(['true(${',FLR_Rule1,'})'],FLR_Rule)
	)
	).
			
get_sub_sent_head_list(SubSentID,Len,ID,SubHeadList):-	
	!,string:term_to_atom(ID,IDAtom), 
	string:term_to_atom(SubSentID,SubSentIDAtom),
	string:concat_atom(['head',IDAtom,'_',SubSentIDAtom],SubHeadPred),
	NextSubSentID is SubSentID + 1,
	(NextSubSentID > Len
	->
	SubHeadList = [SubHeadPred]
	;
	get_sub_sent_head_list(NextSubSentID,Len,ID,RestSubHeadList),
	SubHeadList = [SubHeadPred|RestSubHeadList]
	).	

get_sub_sent_rule([Index|RestIndexList],[SubRuleHead|RestSubRuleHead],Conds,DepVarList,Flag,World,FLR_Body1,RuleLabel,SubSentRuleList):-
	!,get_phrase_conds(Index,Conds,SubSentConds),
	util_cond_to_flr_labelled_rule(drs([],SubSentConds),World,DepVarList,AntecedentList,Consequent,LastDepVarList),
	string:concat_atom(LastDepVarList,',',LastDepVarListAtom),
	string:concat_atom([FLR_Body1|AntecedentList],',',FLR_Body),
	string:concat_atom([RuleLabel,SubRuleHead,'(',LastDepVarListAtom,'):-',FLR_Body],SubSentRule1),
	string:concat_atom([Consequent,':-',SubRuleHead,'(',LastDepVarListAtom,')'],SubSentRule2),
	get_sub_sent_rule(RestIndexList,RestSubRuleHead,Conds,DepVarList,Flag,World,FLR_Body1,RuleLabel,RestSubSentRuleList),
	SubSentRuleList = [SubSentRule1,SubSentRule2|RestSubSentRuleList].
	
get_sub_sent_rule([],[],_,_,_,_,_,_,[]).	
	
util_cond_to_flr_labelled_rule(drs(Refs2,Conds2),World,DepVarList,AntecedentList,Consequent,LastDepVarList):-	
	((Refs2 = [],Conds2 = [Drs1 => Drs2])
	->
	extract_antecedent(Conds2,World,DepVarList,AntecedentList,Consequent,LastDepVarList)
	;
	skolemize_ergo_vars(Refs2,Conds2,DepVarList,ExistsQuant,_,0),
	cond_to_flr(Conds2,World,Consequent,DepVarList,0,_),
	AntecedentList = [],
	LastDepVarList = DepVarList
	).
	
cond_to_flr(RuleLabel(drs(Refs1,Conds1) => drs(Refs2,Conds2)),World,FLR_Rule,DepVarList,Flag,_):-
	!,process_refs(Refs1,ForallQuant,ForallVarList),
	basics:append(DepVarList,ForallVarList,NewDepVarList),
	cond_to_flr(Conds1,World,FLR_Body,NewDepVarList,1,_),
	%process_refs(Refs2,ExistsQuant,_),
	findall(STList,gl_syntax_tree(STList),[STList]),
	RuleLabel = r(ID),
	get_syntax_tree_from_sent_list(STList,ID,ST),
	(is_sent_conjunction(ST)
	->
	get_phrase_index_list(ST,_,IndexList),
	basics:length(IndexList,IndexListLen),
	get_sub_sent_head_list(1,IndexListLen,ID,SubHeadList),
	skolemize_ergo_vars(Refs2,Conds2,NewDepVarList,ExistsQuant,_,Flag),
	string:term_to_atom(RuleLabel,TempRuleLabelAtom),
	string:concat_atom(['@{',TempRuleLabelAtom,'} '],RuleLabelAtom),
	get_sub_sent_rule(IndexList,SubHeadList,Conds2,NewDepVarList,Flag,World,FLR_Body,RuleLabelAtom,SubSentRuleList),
	string:concat_atom(SubSentRuleList,'.\n',FLR_Rule)
	;	
	util_cond_to_flr_labelled_rule(drs(Refs2,Conds2),World,NewDepVarList,AntecedentList,Consequent,LastDepVarList),	
	string:term_to_atom(RuleLabel,RuleLabelAtom),
	string:term_to_atom(ID,IDAtom),
	(ForallQuant == ''
	->
	string:concat_atom([FLR_Body|AntecedentList],',',New_FLR_Body),
	string:concat_atom(['@{',RuleLabelAtom,'} ','head',IDAtom,':-',New_FLR_Body],FLR_Rule1),
	string:concat_atom([Consequent,':-','head',IDAtom],FLR_Rule2),
	string:concat_atom([FLR_Rule1,'.\n',FLR_Rule2],FLR_Rule)
	;
	ForallQuant \== ''
	->
	string:concat_atom([FLR_Body|AntecedentList],',',New_FLR_Body),
	string:concat_atom(LastDepVarList,',',LastDepVarListAtom),
	string:concat_atom(['@{',RuleLabelAtom,'} ','head',IDAtom,'(',LastDepVarListAtom,')',':-',New_FLR_Body],FLR_Rule1),
	string:concat_atom([Consequent,':-','head',IDAtom,'(',LastDepVarListAtom,')'],FLR_Rule2),
	string:concat_atom([FLR_Rule1,'.\n',FLR_Rule2],FLR_Rule)
	)).

*/
%% END Commenting 	
	
%% prepare sub_head predicates	
%% search each sub-drs-conditions	
%% call cond_to_flr to process these sub-conditions
	
	
%%how to specify defeasible rule??	
%cond_to_flr(can(DRS),World1,CAN_FLR_Rule):-
%	!,process_refs([Var],World2,_),
%	drs_to_flr(DRS,World2,FLR_Rule),
%	string:concat_atom(['exists(',World2,')^(','accessibility_relation(',World1,',',World2,'),',FLR_Rule,')'],CAN_FLR_Rule).

%cond_to_flr(must(DRS),World1,MUST_FLR_Rule):-
%	!,process_refs([Var],World2,_),
%	drs_to_flr(DRS,World2,FLR_Rule),
%	string:concat_atom(['forall(',World2,')^(',FLR_Rule,'<==','accessibility_relation(',World1,',',World2,'))'],MUST_FLR_Rule).

%cond_to_flr(World2:DRS,World1,World_FLR_Rule):-
%	!,drs_to_flr(DRS,World2,FLR_Rule),
%	string:concat_atom(['accessibility_relation(',World1,',',World2,'),',FLR_Rule],World_FLR_Rule).	
	
cond_to_flr([Condition],World,FLR_Predicate,DepVarList,Flag,_):-
	!,cond_to_flr(Condition,World,FLR_Predicate,DepVarList,Flag,_).
	
cond_to_flr([Condition1,Condition2|RestConditions],World,FLR_Predicates,DepVarList,Flag,_):-
	!,cond_to_flr(Condition1,World,FLR_Predicate,DepVarList,Flag,_),
	cond_to_flr([Condition2|RestConditions],World,Rest_FLR_Predicates,DepVarList,Flag,_),
	string:concat_atom([FLR_Predicate,Rest_FLR_Predicates],',',FLR_Predicates).

cond_to_flr(cancel(RuleLabel) - Index, World, FLR_Predicate,_,_,_):-
	!,string:term_to_atom(RuleLabel,RuleLabelAtom),
	string:concat_atom(['\cancel(',RuleLabelAtom,')'],FLR_Predicate).	

%%bug: formula may contain 0 argument
cond_to_flr(formula(L) - Index, World, FLR_Predicate,_,_,_):-
	!,extract_var_from_formula(L,VarList),
	inc_formula_id_ctr(NextID),
	assert(gl_formula_id_ctr(NextID)),
	(VarList = []
	->
	string:concat_atom(['formula',NextID],FLR_Predicate),
	string:concat_atom(['formula',NextID,':-'|L],Aux_Rule)
	;
	string:concat_atom(VarList,',',VarListAtom),
	string:concat_atom(['formula',NextID,'(',VarListAtom,')'],FLR_Predicate),
	string:concat_atom(['formula',NextID,'(',VarListAtom,')',':-'|L],Aux_Rule)
	),
	assert(aux_axiom(Aux_Rule)).
	
extract_var_from_formula([],[]):-!.
extract_var_from_formula([Arg|RestArgs],VarList):-!,
	((number(Arg);var(Arg))
	->
	VarList = RestVars
	;
	atom_codes(Arg,[63|_])
	->
	VarList = [Arg|RestVars]
	;
	VarList = RestVars
	),extract_var_from_formula(RestArgs,RestVars).

cond_to_flr(BasicCondition - Index, World, FLR_Predicate,_,_,_):-
	!,BasicCondition =.. [Functor|Arguments],
	reformulate_int_or_string(Arguments,NewArguments),
    NewBasicCondition =..[Functor|[World|NewArguments]],
    string:term_to_atom(NewBasicCondition, NewBasicConditionAtom),
	string:term_to_atom(Index, IndexAtom),
	string:concat_atom([NewBasicConditionAtom],FLR_Predicate).	

cond_to_flr(RuleLabel(BasicCondition - Index), World, FLR_Predicate,_,_,_):-
	!,BasicCondition =.. [Functor|Arguments],
	reformulate_int_or_string(Arguments,NewArguments),
    NewBasicCondition =..[Functor|[World|NewArguments]],
    string:term_to_atom(NewBasicCondition, NewBasicConditionAtom),
	string:term_to_atom(Index, IndexAtom),
	string:term_to_atom(RuleLabel,RuleLabelAtom),
	string:concat_atom(['(@{',RuleLabelAtom,'} ',NewBasicConditionAtom,')'],FLR_Predicate).	

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
		
%cond_to_flr_1([r(ID)(Condition)],World,FLR_Predicate,Stream):-
%	!,cond_to_flr(r(ID)(Condition),World,FLR_Predicate1),
%	write(Stream,FLR_Predicate1),
%	write(Stream,'.\n'),
%	string:concat_atom([FLR_Predicate1,'.\n'],FLR_Predicate).

%% translate conditions in the first level into facts and rules.
%% +List of conditions, +World Skolem (Possible World Semantics), -Concatenation of predicates in string format, +Fileoutput stream		
cond_to_flr_1([Condition],World,FLR_Predicate,Stream):-
	!,cond_to_flr(Condition,World,FLR_Predicate1,[],0,0),
	write(Stream,FLR_Predicate1),
	write(Stream,'.\n'),
	string:concat_atom([FLR_Predicate1,'.\n'],FLR_Predicate).

%cond_to_flr_1([r(ID)(Condition1),Condition2|RestConditions],World,FLR_Predicates,Stream):-
%	!,cond_to_flr(r(ID)(Condition1),World,FLR_Predicate1),
%	write(Stream,FLR_Predicate1),
%	write(Stream,'.\n'),
%	cond_to_flr_1([Condition2|RestConditions],World,Rest_FLR_Predicates,Stream),
%	string:concat_atom([FLR_Predicate1,Rest_FLR_Predicates],'.\n',FLR_Predicates).
	
cond_to_flr_1([Condition1,Condition2|RestConditions],World,FLR_Predicates,Stream):-
	!,cond_to_flr(Condition1,World,FLR_Predicate1,[],0,0),
	write(Stream,FLR_Predicate1),
	write(Stream,'.\n'),
	string:concat_atom([FLR_Predicate1],FLR_Predicate),
	cond_to_flr_1([Condition2|RestConditions],World,Rest_FLR_Predicates,Stream),
	string:concat_atom([FLR_Predicate,Rest_FLR_Predicates],'.\n',FLR_Predicates).

%% process copula(to be) predicates
%is_var_in_list(_,[]):-!,fail.
%is_var_in_list(Var,[H|T]):-Var==H,!.
%is_var_in_list(Var,[H|T]):-Var \== H,is_var_in_list(Var,T).
%rm_var_in_list(_,[]):-!,fail.
%rm_var_in_list(Var,[H|T],T):-Var==H,!.
%rm_var_in_list(Var,[H|T],[H|T1]):-Var \== H,rm_var_in_list(Var,T,T1).
%is_copula_in_list([],_):-!,fail.
%is_copula_in_list([predicate(_,be,_,_)-Index|T],predicate(_,be,_,_)):-!.
%is_copula_in_list([H|T],R):-H \= predicate(_,be,_,_),is_copula_in_list(T,R).
%is_object_in_list(Var,[]):-!,fail.
%is_object_in_list(Var1,[object(Var2,Noun,_,_,_,_)-Index|T]):-writeln('pass0'),Var1==Var2,writeln('pass1'),!.
%is_object_in_list(Var1,[object(Var2,Noun,_,_,_,_)-Index|T]):-Var1 \= Var2,is_object_in_list(T,R).

inc_var_ctr(NextID):-
	findall(ID,gl_var_ctr(ID),IDList),
	max_list(IDList,CurID), NextID is CurID + 1.
	
inc_formula_id_ctr(NextID):-
	findall(ID,gl_formula_id_ctr(ID),IDList),
	max_list(IDList,CurID), NextID is CurID + 1.