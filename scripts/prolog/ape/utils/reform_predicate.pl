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


%% process copula and formula
			
reform_drs(drs(Refs,Conds),drs(Refs,NewConds)) :-
  !,reform_cond(Conds,NewConds).

reform_cond(-Drs,-NewDrs):-
	!,reform_drs(Drs,NewDrs).

reform_cond(RuleLabel(-Drs),RuleLabel(-NewDrs)):-
	!,reform_drs(Drs,NewDrs).

reform_cond(~Drs,~NewDrs):-
	!,reform_drs(Drs,NewDrs).

reform_cond(RuleLabel(~Drs),RuleLabel(~NewDrs)):-
	!,reform_drs(Drs,NewDrs).
	
reform_cond(drs(Ref1,Conds1) v Drs2,drs(Ref1,NewConds1) v NewDrs2):-
	!,reform_cond(Conds1,NewConds1),
	reform_drs(Drs2,NewDrs2).

reform_cond(RuleLabel(drs(Ref1,Conds1) v Drs2),RuleLabel(drs(Ref1,NewConds1) v NewDrs2)):-
	!,reform_cond(Conds1,NewConds1),
	reform_drs(Drs2,NewDrs2).
	
reform_cond(drs(Refs1,Conds1) => drs(Refs2,Conds2),drs(Refs1,NewConds1) => drs(Refs2,NewConds2)):-
	!,reform_cond(Conds1,NewConds1),
	reform_cond(Conds2,NewConds2).

reform_cond(drs(Refs1,Conds1) ~~> drs(Refs2,Conds2),drs(Refs1,NewConds1) ~~> drs(Refs2,NewConds2)):-
	!,reform_cond(Conds1,NewConds1),
	reform_cond(Conds2,NewConds2).
	
reform_cond(RuleLabel(drs(Refs1,Conds1) => drs(Refs2,Conds2)),RuleLabel(drs(Refs1,NewConds1) => drs(Refs2,NewConds2))):-
	!,reform_cond(Conds1,NewConds1),
	reform_cond(Conds2,NewConds2).

reform_cond(RuleLabel(drs(Refs1,Conds1) ~~> drs(Refs2,Conds2)),RuleLabel(drs(Refs1,NewConds1) ~~> drs(Refs2,NewConds2))):-
	!,reform_cond(Conds1,NewConds1),
	reform_cond(Conds2,NewConds2).
		
reform_cond(can(DRS),can(NewDRS)):-
	!,reform_drs(DRS,NewDRS).

reform_cond(must(DRS),must(NewDRS)):-
	!,reform_drs(DRS,NewDRS).

reform_cond(World2:DRS,World2:NewDRS):-
	!,reform_drs(DRS,NewDRS).
	
reform_cond([Condition],Result):-
	!,reform_cond(Condition,NewCondition),
	(NewCondition = '\\true'-Index
	->
	Result = []
	;
	Result = [NewCondition]
	).
	
reform_cond([Condition1,Condition2|RestConditions],Result):-
	!,reform_cond(Condition1,NewCondition1),
	(NewCondition1 = '\\true'-Index
	->
	reform_cond([Condition2|RestConditions],Result)
	;
	reform_cond([Condition2|RestConditions],NewRestConditions),
	Result = [NewCondition1|NewRestConditions]
	).

reform_cond(predicate(X,be,Y) - Index,'\\true'-Index):-!,X=Y.
reform_cond(predicate(Var,be,X,Y) - Index,'\\true'-Index):-var(X),var(Y),!,X=Y,Var = '?X'.
reform_cond(predicate(Var,be,X,Y) - Index,predicate(Var,be,X,Y) - Index):-nonvar(X),nonvar(Y),!.
reform_cond(predicate(Var,be,X,Y) - Index,'\\true'-Index):-(nonvar(X);nonvar(Y)),!,X=Y,Var = '?X'.
reform_cond(RuleLabel(predicate(X,be,Y) - Index),RuleLabel('\\true'-Index)):-!,X=Y.
reform_cond(RuleLabel(predicate(Var,be,X,Y) - Index),RuleLabel('\\true'-Index)):-var(X),var(Y),!,X=Y,Var = '?X'.
reform_cond(RuleLabel(predicate(Var,be,X,Y) - Index),RuleLabel(predicate(Var,be,X,Y) - Index)):-nonvar(X),nonvar(Y),!.
reform_cond(RuleLabel(predicate(Var,be,X,Y) - Index),RuleLabel('\\true'-Index)):-(nonvar(X);nonvar(Y)),!,X=Y,Var = '?X'.


%reform_cond(formula(Arg1,=,Arg2),formula([Arg1,=,Arg2])):-!.%%append
%reform_cond(formula(Arg1,\=,Arg2),formula([Arg1,\=,Arg2])):-!.
%reform_cond(formula(Arg1,<,Arg2),formula(Arg1 < Arg2)):-!.
%reform_cond(formula(Arg1,>,Arg2),formula(Arg1 > Arg2)):-!.
%reform_cond(formula(Arg1,=<,Arg2),formula(Arg1 =< Arg2)):-!.
%reform_cond(formula(Arg1,>=,Arg2),formula(Arg1 >= Arg2)):-!.

reform_cond(formula(Arg1,Op,Arg2) - Index,formula(L) - Index):-!,
	prefix_to_infix_expr(Arg1,NewArg1),prefix_to_infix_expr(Arg2,NewArg2),
	basics:append(NewArg1,[Op|NewArg2],L).
	
prefix_to_infix_expr(Arg,[Arg]):-var(Arg),!.
prefix_to_infix_expr(Arg,[Number]):-Arg = int(Number),!.
prefix_to_infix_expr(Arg,L):-Arg = expr(Op,Arg1,Arg2),!,
	prefix_to_infix_expr(Arg1,NewArg1),prefix_to_infix_expr(Arg2,NewArg2),
	basics:append(NewArg1,[Op|NewArg2],L).

reform_cond(BasicCondition - Index,BasicCondition - Index):-!.	
reform_cond(RuleLabel(BasicCondition - Index),RuleLabel(BasicCondition - Index)):-!.