:- import append/3 from basics.
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

resolve_defs(drs(Refs,Conds),drs(Refs,LabeledConds)):- 
	attach_rule_label(Conds,LabeledConds).
	%add_opposes_conds(OpposesConds),
	%append(LabeledConds,OpposesConds,CondsOut).
	
get_drs_sid(drs(Refs,[Condition]), SID) :-
  !,
  get_cond_sid(Condition, SID).

get_drs_sid(drs(Refs,[Condition1,Condition2|Conditions]), SID) :-
  !,
  get_cond_sid(Condition1, SID).

get_cond_sid(~DRS, SID) :-
  !,
  get_drs_sid(DRS, SID).  
  
get_cond_sid(-DRS, SID) :-
  !,
  get_drs_sid(DRS, SID).

get_cond_sid(Drs1 v DRS2, SID) :-
  !,
  get_drs_sid(Drs1, SID).

get_cond_sid(Drs1 => DRS2, SID) :-
  !,
  get_drs_sid(Drs1, SID).

get_cond_sid(Label:DRS, SID) :-
  !,
  get_drs_sid(DRS, SID).	

get_cond_sid(can(DRS), SID) :-
  !,
  get_drs_sid(DRS, SID).	
  
get_cond_sid(must(DRS), SID) :-
  !,
  get_drs_sid(DRS, SID).	
  
get_cond_sid(Condition-SID/_,SID).
  
attach_rule_label([Condition|RestConditions],[LabeledCondition|LabeledRestConditions]):-
(
	Condition = _-SID/_,def_rule(SID)
	->
	LabeledCondition = r(SID)(Condition)
	;
	Condition = drs(Ref1,Cond1) => drs(Ref2,Cond2), get_drs_sid(drs(Ref1,Cond1),SID),def_rule(SID)
	->
	LabeledCondition = r(SID)(Condition)
	;
	Condition = Drs1 v Drs2, get_drs_sid(Drs1,SID),def_rule(SID)
	->
	LabeledCondition = r(SID)(Condition)
	;
	Condition = -drs(Refs,Conds),get_drs_sid(drs(Refs,Conds),SID),def_rule(SID)
	->
	LabeledCondition = r(SID)(Condition)
	;
	Condition = ~drs(Refs,Conds),get_drs_sid(drs(Refs,Conds),SID),def_rule(SID)
	->
	LabeledCondition = r(SID)(Condition)
	;
	Condition = Label:drs(Refs,Conds),get_drs_sid(drs(Refs,Conds),SID),def_rule(SID)
	->
	LabeledCondition = r(SID)(Label:drs(Refs,Conds))
	;
	Condition = can(drs(Refs,Conds)),get_drs_sid(drs(Refs,Conds),SID),def_rule(SID)
	->
	LabeledCondition = r(SID)(can(drs(Refs,Conds)))
	;
	Condition = must(drs(Refs,Conds)),get_drs_sid(drs(Refs,Conds),SID),def_rule(SID)
	->
	LabeledCondition = r(SID)(must(drs(Refs,Conds)))
	;
	LabeledCondition = Condition
),attach_rule_label(RestConditions,LabeledRestConditions).

attach_rule_label([],[]).		
		
%% the following code need to be changed to ergo program
%%		
%add_opposes_conds(OpposesList):-
%	findall(exception_list(X,Y),exception_list(X,Y),ExceptionList),
%	build_opposes_list(ExceptionList,OpposesList).

%build_opposes_list([exception_list(X,Y)|Rest],[]):-
%	(\+ labeled_rule_heads(X,Drs1);\+ labeled_rule_heads(Y,Drs2)),
%	add_error_message(syntax, X-2, '', 'Incorrect exception sentence id.'),!.

%% does not handle negations
	
%build_opposes_list([exception_list(X,Y)|Rest],[overrides('r'(X),'r'(Y)),FinalOpposesList]):-
%	findall(Head1,labeled_rule_heads(X,Head1),HeadList1), findall(Head2,labeled_rule_heads(Y,Head2),HeadList2),
%	(basics:length(HeadList1,Len1), Len1 > 1
%	->
%	PredicateList1 = HeadList1
%	;
%	HeadList1 = [drs(Refs1,Conds1)],PredicateList1 = Conds1
%	),
%	(basics:length(HeadList2,Len2), Len2 > 1
%	->
%	PredicateList2 = HeadList2
%	;
%	HeadList2 = [drs(Refs2,Conds2)],PredicateList2 = Conds2
%	),
%	cartesian_product_conds(PredicateList1,PredicateList2,OpposesList),
%	build_opposes_list(Rest,RestOpposesList),
%	basics:append(OpposesList,RestOpposesList,FinalOpposesList).

%build_opposes_list([],[]).

%% simple conditions may include negations

%cartesian_product_conds([Cond1],[Cond2],opposes(Cond1,Cond2)):-!.

%cartesian_product_conds([Cond1],[Cond2,Cond3|RestConds],[opposes(Cond1,Cond2)|RestOpposes]):-
%	!,cartesian_product_conds([Cond1],[Cond3|RestConds],RestOpposes).

%cartesian_product_conds([Cond1|RestConds1],[Cond2|RestConds2],OpposesList):-
%	!,cartesian_product_conds([Cond1],[Cond2|RestConds2],OpposesList1),
%	cartesian_product_conds(RestConds1,[Cond2|RestConds2],RestOpposesLists),
%	basics:append(OpposesList1,RestOpposesLists,OpposesList).
	
	