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


drs_xml(DRS):-
	open('d:\\drs_xml.txt',write,Stream),
	drs_to_xml(DRS,Stream,0),
	close(Stream).

drs_to_xml(drs(Refs,Conds),Stream,Level):-	
	!,cond_to_xml(Conds,Stream,Level).
	
cond_to_xml(drs(Refs1,Conds1) => drs(Refs2,Conds2),Stream,Level):-
	!,write(Stream,Level),write(Stream,'|IF\n'),
	NextLevel is Level + 1,
	drs_to_xml(drs(Refs1,Conds1),Stream,NextLevel),
	write(Stream,Level),write(Stream,'|THEN\n'),
	drs_to_xml(drs(Refs2,Conds2),Stream,NextLevel).
		
cond_to_xml([Condition],Stream,Level):-
	!,cond_to_xml(Condition,Stream,Level).
	
cond_to_xml([Condition1,Condition2|RestConditions],Stream,Level):-
	!,cond_to_xml(Condition1,Stream,Level),
	cond_to_xml([Condition2|RestConditions],Stream,Level).
	
cond_to_xml(BasicCondition - Index, Stream, Level):-
	!,write(Stream,Level),
	write(Stream,'|'),
	write(Stream,BasicCondition),
	write(Stream,'\n').
	