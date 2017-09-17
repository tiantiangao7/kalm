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

:- dynamic gl_cnf_var_ctr/1.

%% Convert DRS to CNF 

% drs_to_cnf(+DRS,-CNF)
% 
% translate DRS to conjunctive normal form
% 
% @param DRS is an Attempto DRS representing a rule to be proved
%	1. p1,p2,...,pn,[\neg exists(...)^(q1,q2,...,qn)] ==> r1,r2,...,rn
%   2. p1,p2,...,pn,[\neg exists(...)^(q1,q2,...,qn)] ==> 
%									r1,r2,...,rn,\neg exists(...)^(q1,q2,...,qn)
%   3. p1,p2,...,pn,[\neg exists(...)^(q1,q2,...,qn)] ==> \neg exists(...)^(r1,r2,...,rn)
%
% @param CNF is the conjuctive normal form of first-order rule
% 

drs_cnf(DRS,CNF_List):-
	drs_cnf_init,
	drs_pnf(DRS,'sk0',PNF),
	remove_pnf_quantifier(PNF,[],PNF1),
	cnf(PNF1,CNF),
	cnf_list(CNF,CNF_List).

%
% initialize variable counters for drs_to_cnf
%
	
drs_cnf_init:-
	(\+ gl_cnf_var_ctr(_)
	->
	assert(gl_cnf_var_ctr(0))
	;
	true).	

% remove_pnf_quantifier(+PNF,+Dep_List,-QF_Fol)
%
% remove quantifiers from prenex normal form:
%	1. forall(Var) -> 'Y2'
%   2. exists(Var) -> 'cnfsk5(Y1,Y2,Y3)'
%
% @param PNF is prenex nornal form
% @param Dep_List is a list of forall variables found so far
% @param QF_Fol is FOL formula with no quantifiers
%

remove_pnf_quantifier(forall(X,Rest_Fol),Dep_List, QF_Fol):-
	var(X),!,
	inc_cnf_var_ctr(NextID),
	assert(gl_cnf_var_ctr(NextID)),
	string:concat_atom(['?Y',NextID],X),
	basics:append(Dep_List,[X],Dep_List2),
	remove_pnf_quantifier(Rest_Fol,Dep_List2, QF_Fol).

remove_pnf_quantifier(forall(X,Rest_Fol),Dep_List, QF_Fol):-
	remove_pnf_quantifier(Rest_Fol,Dep_List, QF_Fol).

remove_pnf_quantifier(exists(X,Rest_Fol),Dep_List, QF_Fol):-
	var(X),!,
	inc_cnf_var_ctr(NextID),
	assert(gl_cnf_var_ctr(NextID)),
	(Dep_List \= []
	->
	string:concat_atom(Dep_List,',',T),
	string:concat_atom(['cnfsk',NextID,'(',T,')'],X)
	;
	string:concat_atom(['cnfsk',NextID],X)
	),
	remove_pnf_quantifier(Rest_Fol,Dep_List, QF_Fol).

remove_pnf_quantifier(exists(X,Rest_Fol),Dep_List, QF_Fol):-
	remove_pnf_quantifier(Rest_Fol,Dep_List, QF_Fol).
		
remove_pnf_quantifier(A & B,_, A & B).
remove_pnf_quantifier(A v B,_, A v B).
remove_pnf_quantifier(Predicate - Index,_,Predicate - Index).
remove_pnf_quantifier(-(Predicate - Index),_,-(Predicate - Index)).

% distribute_fol(+Disjunction,-DistributedForm,-Flag)
%
% distribute v into &
%
% @param Disjunction is a disjunction of formula
% @param DistributedForm is the formula after distributing v into &
% @param Flag = true indicates that v is disbuted into &, 
%		 Flat = false indicates that distribution is not applicable
%

distribute_fol((X & Y) v Z, (X v Z) & (Y v Z),true) :- !.
distribute_fol(X v (Y & Z), (X v Y) & (X v Z),true) :- !.
distribute_fol(X,X,false).

% cnf(+Formula,-CNF)
%
% convert a first-order formula into cnf
%
% @param Formula is first-order formula
% @param CNF is the corresponding conjunctive normal form
%
cnf(A & B, A1 & B1):-!, 
	cnf(A,A1),
	cnf(B,B1).

cnf((X v Y),G) :-!,
   cnf(X,A),
   cnf(Y,B),
   distribute_fol((A v B),F,Flag),
   (Flag -> cnf(F,G)
   ;
   G = F
   ).

cnf(X,X).

% flatten_disjunction(+Disjunction,-List)
%
% flatten a disjunction of (positive/negative) predicates into a list
% 
% @param Disjuction is a disjunction of predicates, e.g., (A v B) v C
% @param List stores all predicates of the disjunction
%

flatten_disjunction(A v B, L):-!,
	flatten_disjunction(A, L1),
	flatten_disjunction(B, L2),
	basics:append(L1, L2, L).

flatten_disjunction(Predicate - Index, [Predicate_Atom1]):-
	Predicate =.. [Functor|Arguments],
	reformulate_int_or_string(Arguments,NewArguments),
    Predicate2 =.. [Functor|NewArguments],
	string:term_to_atom(Predicate2,Predicate_Atom),
	string:concat_atom(['${',Predicate_Atom,'}'],Predicate_Atom1).
	
flatten_disjunction(-(Predicate - Index), [Predicate_Atom1]):-
	Predicate =.. [Functor|Arguments],
	reformulate_int_or_string(Arguments,NewArguments),
    Predicate2 =.. [Functor|NewArguments],
	string:term_to_atom(Predicate2,Predicate_Atom),
	string:concat_atom(['${','\\neg ',Predicate_Atom,'}'],Predicate_Atom1).

% cnf_list(+CNF,-List)
% 
% convert cnf into a list of lists where each element is a disjunction of predicates
%
% @param CNF is formula in conjunctive normal form
% @param List is a list of lists where each element is a disjunction
%

cnf_list(X & Y, L) :-!,
	cnf_list(X, L1),
	cnf_list(Y, L2),
	basics:append(L1, L2, L).

cnf_list(X, [L1]):- flatten_disjunction(X,L1).

% cnf_fact(+CNF_List,+Rule_Id)
%
% serialize each clause to a fact in knowledge base
%
% CNF_List is a list of lists, each representing a clause

cnf_fact(CNF_List,Rule_Id):-
	open('/Users/tiantiangao/Documents/Research/2015-09-13/ergo/clauses.ergo',
	write,Stream),
	cnf_to_fact(Stream,CNF_List,Rule_Id,1),
	close(Stream).

% cnf_to_fact(+Stream,+Clause_List,+Rule_Id,+Sub_Rule_Id)
%
% represent each clause in clause list as %clause(Rule_Id-Sub_Rule_Id,List)
% where list is a disjunction of literals
% 
% @param Stream is Prolog output stream
% @param Clause_List is a list of lists, each representing a clauses
% @param Rule_Id is assigned to @Clause_List
% @param Sub_Rule_Id is assigned to each clause in @Clause_List

cnf_to_fact(Stream, [Element],Rule_Id,Sub_Rule_Id):-
	!,string:concat_atom([Rule_Id,'-',Sub_Rule_Id],Index),
	write(Stream,'%'),
	write(Stream,clause(Index,Element)),
	write(Stream,'.\n'),
	clause_to_index(Stream,Element,Index).

cnf_to_fact(Stream, [Element1,Element2|RestElements],Rule_Id,Sub_Rule_Id):-
	!,string:concat_atom([Rule_Id,'-',Sub_Rule_Id],Index),
	write(Stream,'%'),
	write(Stream,clause(Index,Element1)),
	write(Stream,'.\n'),
	clause_to_index(Stream,Element1,Index),
	Next_Sub_Rule_Id is Sub_Rule_Id + 1,
	cnf_to_fact(Stream,[Element2|RestElements],Rule_Id,Next_Sub_Rule_Id).

% clause_to_index(+Stream,+Clause,+Index)
%
% Represent each clause as an index form, namely clause(1-1,[${p},${\neg q}]) is
% converted to index_clause(${p},1-1), index_clause(${\neg q},1-1)
% 
% @param Stream is Prolog output stream
% @param Clause is a disjunction of literals
% @param Index is clause id.

clause_to_index(Stream,[Element],Index):-!,
	write(Stream,'%'),
	write(Stream,index_clause(Element,Index)),
	write(Stream,'.\n').

clause_to_index(Stream,[Element1,Element2|RestElements],Index):-!,
	write(Stream,'%'),
	write(Stream,index_clause(Element1,Index)),
	write(Stream,'.\n'),	
	clause_to_index(Stream,[Element2|RestElements],Index).
	
% inc_cnf_var_ctr(-NextID)
%
% Helper function that assigns an index to a new variable
%

inc_cnf_var_ctr(NextID):-
	findall(ID,gl_cnf_var_ctr(ID),IDList),
	max_list(IDList,CurID), NextID is CurID + 1.