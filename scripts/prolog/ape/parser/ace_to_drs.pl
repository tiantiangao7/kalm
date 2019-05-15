% This file is part of the Attempto Parsing Engine (APE).
% Copyright 2008-2013, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
%
% The Attempto Parsing Engine (APE) is free software: you can redistribute it and/or modify it
% under the terms of the GNU Lesser General Public License as published by the Free Software
% Foundation, either version 3 of the License, or (at your option) any later version.
%
% The Attempto Parsing Engine (APE) is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
% PURPOSE. See the GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with the Attempto
% Parsing Engine (APE). If not, see http://www.gnu.org/licenses/.


%:- module(ace_to_drs, [
%		acetext_to_drs/5,       % +Text, -Sentences, -SyntaxTrees, -Drs, -Messages
%		acetext_to_drs/8,       % +Text, +Guess, +Catch, -Sentences, -SyntaxTrees, -Drs, -Messages, -Time
%		aceparagraph_to_drs/6,  % +Text, -Sentences, -SyntaxTrees, -UnresolvedDrs, -Drs, -Messages
%		aceparagraph_to_drs/10  % +Text, +Guess, +Catch, +StartID, -Sentences, -SyntaxTrees, -UnresolvedDrs, -Drs, -Messages, -Time
%	]).

:- import append/3, length/2, member/2, ith/3 from basics.
	
/** <module> ACE to DRS

@author Kaarel Kaljurand
@author Tobias Kuhn
@version 2009-05-21
*/

%:- use_module(error_logger,
%	  [
%	   clear_messages/0,
%	   clear_messages/1,                     % +Type
%	   clear_error_messages/0,
%	   clear_error_messages_sentence/1,      % +Sentence
%	   clear_warning_messages/0,
%	   get_messages/1,                       % -Messages
%	   get_messages_in_xml/1,                % -XmlMessages
%	   get_error_messages/1,                 % -ErrorMessages
%	   get_warning_messages/1,               % -WarningMessages
%	   get_messages_with_type/2,             % +Type, -AllMessages
%	   get_error_messages_with_type/2,       % +Type, -ErrorMessages
%	   get_warning_messages_with_type/2,     % +Type, -WarningMessages
%	   add_error_messagelist/4,              % +Type, +Position, +Subject, +DescriptionList
%	   add_error_message/4,                  % +Type, +Position, +Subject, +Description
%	   add_error_message_once/4,             % +Type, +Position, +Subject, +Description
%	   add_warning_message/4,                % +Type, +Position, +Subject, +Description
%	   add_warning_message_once/4,           % +Type, +Position, +Subject, +Description
%	   add_messages/1,                       % +MessageList
%	   is_error_message/4,                   % +Type, +Position, +Subject, +Description
%	   messages_xmlmessages/2                % +Messages, -XmlMessages
%	  ]).
	  
%:- use_module(drs_reverse, [
%		drs_reverse/2
%	]).

%:- use_module(ape_utils, [
%		cpu_time/2,
%		handle_unknown_words/4,
%		new_npid/1,
%		b_setval/2,
%		list_close/2,
%		list_of_conds_and_anaphors/3,
%		report_failed_sentence/2
%	]).

%:- use_module(tokenizer).

%:- use_module(tokens_to_sentences, [
%		tokens_to_sentences/2,
%		tokens_to_paragraphs/2
%	]).

%:- use_module(refres, [
%		resolve_anaphors/2
%	]).

%:- debug(result).


%% acetext_to_drs(+Text, -Sentences, -SyntaxTrees, -Drs, -Messages).
%% acetext_to_drs(+Text, +Guess, +Catch, -Sentences, -SyntaxTrees, -Drs, -Messages, -Time).
%
% Provides an interface to the parser for parsing an ACE text that consists of one or more
% paragraphs. These predicates will always succeed even if the parser fails.
%
% Examples:
%
%==
% acetext_to_drs('Every man waits.', Sentences, SyntaxTrees, Drs, Messages)
% acetext_to_drs('Every man waits.', on, off, Sentences, SyntaxTrees, Drs, Messages, Time)
%==
%
% @param Text is an ACE text
% @param Guess is either 'on' or 'off' (default) and defines whether unknown word guessing should be used
% @param Catch is either 'on' or 'off' (default) and defines whether unexpected errors should be catched and put
%   to the list of messages or not
% @param Sentences is the list of sentences (each a list of tokens) in the ACE text
% @param SyntaxTrees is the list of syntax trees (each a complex list) of the ACE text
% @param Drs is the DRS of the ACE text
% @param Messages is the list of error and warning messages that result from the processing

acetext_to_drs(Text, Sentences, SyntaxTrees, Drs, Messages,CorefList) :-
	acetext_to_drs(Text, off, off, Sentences, SyntaxTrees, Drs2, Messages, _, CorefList),
	(Drs2 = drs([],[question(drs(Refs,Conditions))])
	 ->
	 true
	 ;
	 Drs2 = drs(Refs,Conditions)
	),
	reconstruct_object(Conditions,Conditions2),
	Drs = drs(Refs,Conditions2).

acetext_to_drs(Text, Guess, Catch, Sentences, SyntaxTrees, Drs, Messages, [TimeT, TimeP, TimeR],CorefList) :-
	cpu_time(tokenize(Text, Tokens), T),
	tokens_to_paragraphs(Tokens, Paragraphs),
	paragraphs_to_drs(Paragraphs, Guess, Catch, 1, Sentences, SyntaxTrees, Drs, Messages, [TimeTPre, TimeP, TimeR],CorefList),
	clear_messages,
	add_messages(Messages),
	TimeT is T + TimeTPre,
	!.


%% aceparagraph_to_drs(+Text, -Sentences, -SyntaxTrees, -UnresolvedDrs, -Drs, -Messages).
%% aceparagraph_to_drs(+Text, +Guess, +Catch, +StartID, -Sentences, -SyntaxTrees, -UnresolvedDrs, -Drs, -Messages, -Time).
%
% Provides an interface to the parser for parsing an ACE text that consists of one or more
% paragraphs. These predicates will always succeed even if the parser fails.
%
% Examples:
%
%==
% aceparagraph_to_drs('Every man waits.', Sentences, SyntaxTrees, UnresolvedDrs, Drs, Messages)
% aceparagraph_to_drs('Every man waits.', on, off, 1, Sentences, SyntaxTrees, UnresolvedDrs, Drs, Messages, Time)
%==
%
% @param Text is an ACE text
% @param Guess is either 'on' or 'off' (default) and defines whether unknown word guessing should be used
% @param Catch is either 'on' or 'off' (default) and defines whether unexpected errors should be caught and put
%   to the list of messages or not
% @param StartID is the sentence id for the first sentence (default is 1)
% @param Sentences is the list of sentences (each a list of tokens) in the ACE text
% @param SyntaxTrees is the list of syntax trees (each a complex list) of the ACE text
% @param UnresolvedDrs is the DRS of the ACE text with anaphoric references unresolved (pre-refres DRS)
% @param Drs is the DRS of the ACE text
% @param Messages is the list of error and warning messages that result from the processing

aceparagraph_to_drs(Text, Sentences, SyntaxTrees, UnresolvedDrs, Drs, Messages,CorefList) :-
	aceparagraph_to_drs(Text, off, off, 1, Sentences, SyntaxTrees, UnresolvedDrs, Drs, Messages, _,CorefList).

aceparagraph_to_drs(Text, Guess, Catch, StartID, Sentences, SyntaxTrees, UnresolvedDrsCopy, Drs, Messages, Time, CorefList) :-
	Time = [DTokenizer, DParse, DRefres],
	clear_ape_messages,
	catch(
		(
			cpu_time(call_tokenizer(Text, Guess, Sentences, SentencesToParse), DTokenizer),
			cpu_time(call_parser(SentencesToParse, StartID, SyntaxTrees, UnresolvedDrs), DParse),
			(
				UnresolvedDrsCopy \== off
			->
				copy_term(UnresolvedDrs, UnresolvedDrsCopy)
			; true),
			cpu_time(resolve_anaphors(UnresolvedDrs, DrsTmp, CorefList), DRefres),
			(var(SyntaxTrees) -> SyntaxTrees = [] ; true),
			(var(DrsTmp) -> Drs = drs([], []) ; true),
			(is_error_message(_, _, _, _) -> Drs = drs([], []);DrsTmp1 = DrsTmp),
%%			(is_error_message(_, _, _, _) -> Drs = drs([], []);resolve_defs(DrsTmp,DrsTmp1)),
			(is_error_message(_, _, _, _) -> Drs = drs([], []);Drs = DrsTmp1),
			!
		),
		CatchType,
		(
			(
				Catch == on
			->
				Sentences = [],
				SyntaxTrees = [],
				Drs = drs([], []),
				DTokenizer = -1,
				DParse = -1,
				DRefres = -1,
				add_error_message(ape, '', '', 'Fatal error. Please send screenshot to APE developers.')
			;
				throw(CatchType)
			)
		)
	),
	get_messages(Messages),
	!.


% Note: should not fail.
% If sentence splitting fails then the problem must have been that there
% was no sentence end symbol. In this case we return the original
% token list.
call_tokenizer(Text, GuessOnOff, SentencesOutput, SentencesToParse) :-
	(
		is_list(Text) 
	->
		Tokens = Text
	;
		tokenize(Text, Tokens)
	),
	(
		tokens_to_sentences(Tokens, SentencesTmp)
	->
		SentencesToParse = SentencesTmp
	;
		SentencesOutput = [Tokens],
		SentencesToParse = [],
		last(Tokens, LastToken),
		add_error_message(sentence, '', LastToken, 'Every ACE text must end with . or ? or !.')
	).
	
%last([],_):-!,fail.
%last(List,Last):- is_list(List),
%	length(List,Len),ith(Len, List, Element),member(Last,[Element]).

call_parser(Sentences, StartID, Syntaxtrees, DrsReversed) :-
	parse(Sentences, StartID, Syntaxtrees, Drs),
	drs_reverse(Drs, DrsReversed).


%% paragraphs_to_drs(+Paragraphs, +Guess, +Catch, +StartID, -Sentences, -Trees, -Drs, -Messages, -Time).

paragraphs_to_drs([], _, _, _, [], [], drs([],[]), [], [0,0,0],[]) :-
	!.

paragraphs_to_drs([P|Paragraphs], Guess, Catch, StartID, Sentences, Trees, drs(Dom,Conds), Messages, [TimeT, TimeP, TimeR],CorefList) :-
	aceparagraph_to_drs(P, Guess, Catch, StartID, S, T, off, drs(D,C), M, [TT, TP, TR],CorefList1),
	length(S, SentenceCount),
	NewStartID is StartID + SentenceCount,
	(
		is_error_message(_, _, _, _)
	->
		Sentences = S,
		Trees = T,
		Dom = D,
		Conds = C,
		Messages = M,
		TimeT = TT,
		TimeP = TP,
		TimeR = TR,
		CorefList = CorefList1
	;
		paragraphs_to_drs(Paragraphs, Guess, Catch, NewStartID, SentencesR, TreesR, drs(DomR, CondsR), MessagesR, [TimeTR, TimePR, TimeRR],CorefList2),
		append(S, SentencesR, Sentences),
		append(T, TreesR, Trees),
		append(D, DomR, Dom),
		append(C, CondsR, Conds),
		append(M, MessagesR, Messages),
		TimeT is TT + TimeTR,
		TimeP is TP + TimePR,
		TimeR is TR + TimeRR,
		basics:append(CorefList1,CorefList2,CorefList)
	),
	!.


clear_ape_messages :-
	clear_messages(character),
	clear_messages(word),
	clear_messages(sentence),
	clear_messages(anaphor),
	clear_messages(pronoun).

% Note: acetext_to_drs_training is used for parsing in the training phase.
%       Each occurence of a named entity is represented by a different variable X:
%       object(X,Lexem,named,...)-Index
acetext_to_drs_training(Text, Sentences, SyntaxTrees, Drs, Messages,CorefList) :-
	acetext_to_drs(Text, off, off, Sentences, SyntaxTrees, Drs, Messages, _, CorefList).

% Note: use the same constant named(Lexem) to represent named entities
% Todo: need to process implication, disjunction, and negation	     
reconstruct_object([object(V,Lexem,named,V3,V4,V5)-Index|Rest],
    [object(V,Lexem,named,V3,V4,V5)-Index|Rest2]) :-
    V = named(Lexem),
    !,
    reconstruct_object(Rest,Rest2).

reconstruct_object([drs(R1,C1) => drs(R2,C2)|Rest],
    [drs(R1,NC1) => drs(R2,NC2)|Rest2]) :-
    reconstruct_object(C1,NC1),
    reconstruct_object(C2,NC2),
    reconstruct_object(Rest,Rest2).

reconstruct_object([-drs(R1,C1)|Rest],[-drs(R1,NC1)|Rest2]) :-
    reconstruct_object(C1,NC1),
    reconstruct_object(Rest,Rest2).

reconstruct_object([Predicate-Index|Rest],[Predicate-Index|Rest2]) :-
    reconstruct_object(Rest,Rest2).    

reconstruct_object([],[]).