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


%:- module(clex, [
%		clex_switch/1,     % ?Switch
%		set_clex_switch/1  % +Switch
%	]).


/** <module> Common Lexicon Interface

This module contains the predicates for the management of the common lexicon that is compiled into
the executable.

@author Tobias Kuhn
@version 2008-07-17
*/
:- import member/2 from basics.
:- dynamic clex_switch/1.

clex_switch(on).


%% set_clex_switch(+Switch)
%
% This predicate switches clex on (Switch='on') or off (Switch='off').

set_clex_switch(Switch) :-
    member(Switch, [on, off]),
    retractall(clex_switch(_)),
    assert(clex_switch(Switch)).
