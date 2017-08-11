:- module(omdb_query, [
	retrieval_query/2,
	search_query/2
]).

:- use_module(library(lists), [member/2, union/3]).
:- use_module(library(apply), [maplist/2, maplist/3]).
:- use_module(library(error), [must_be/2]).
:- use_module(library(uri), [uri_encoded/3]).


/** <module> OMDB URL query construction
This module is responsible for taking user supplied API parameters and
constructing the URL responsible for the actual requests to the server.

@author Ebrahim Azarisooreh
@license MIT
*/


retrieval_option_set([
	id=_
	,title=_
	,'type'=_
	,year=_
	,plot=_
	,tomatoes=_
	,callback=_
	,version=_ ]).

search_option_set([
	title=_
	,'type'=_
	,year=_
	,page=_
	,callback=_
	,version=_
]).

%--------------------------------------------------------------------------------%
% Interface
%--------------------------------------------------------------------------------%


retrieval_query(Args, Template) :-
	pretreat_params(retrieval, Args, Treated),
	phrase(omdb_retrieval(Query), Treated),
	format(string(Template), "~w&~w&~w&~w&~w&~w&~w&~w", Query).


search_query(Args, Template) :-
	pretreat_params(search, Args, Treated),
	phrase(omdb_search(Query), Treated),
	format(string(Template), "~w&~w&~w&~w&~w&~w", Query).


pretreat_params(SetType, Params, Treated) :-
	option_set(SetType, Set, OptionType),
	union(Params, Set, Union),
	maplist(set_default(OptionType), Union),
	maplist(encode_string, Union, Treated).


set_default(OptionType, K=V) :-
	must_be(OptionType, K),
	ignore(V="").

encode_string(X=Y, X=S) :-
	uri_encoded(fragment, Y, E),
	atom_string(E, S).


option_set(retrieval, Set, retrieval_option) :-
	retrieval_option_set(Set).

option_set(search, Set, search_option) :-
	search_option_set(Set).


%--------------------------------------------------------------------------------%
% Argument Construction (retrievals)
%--------------------------------------------------------------------------------%


omdb_retrieval([]) --> eos.
omdb_retrieval([i=Value|Rest]) -->
	[id=Value],
	omdb_retrieval(Rest).

omdb_retrieval([t=Value|Rest]) -->
	[title=Value],
	omdb_retrieval(Rest).

omdb_retrieval(['type'=Value|Rest]) -->
	['type'=Value],
	omdb_retrieval(Rest).

omdb_retrieval([y=Value|Rest]) -->
	[year=Value],
	omdb_retrieval(Rest).

omdb_retrieval([plot=Value|Rest]) -->
	[plot=Value],
	omdb_retrieval(Rest).

omdb_retrieval([tomatoes=Value|Rest]) -->
	[tomatoes=Value],
	omdb_retrieval(Rest).

omdb_retrieval([callback=Value|Rest]) -->
	[callback=Value],
	omdb_retrieval(Rest).

omdb_retrieval([v=Value|Rest]) -->
	[version=Value],
	omdb_retrieval(Rest).


%--------------------------------------------------------------------------------%
% Argument Construction (Searches)
%--------------------------------------------------------------------------------%


omdb_search([]) --> eos.
omdb_search([s=Value|Rest]) -->
	[title=Value],
	omdb_search(Rest).

omdb_search(['type'=Value|Rest]) -->
	['type'=Value],
	omdb_search(Rest).

omdb_search([y=Value|Rest]) -->
	[year=Value],
	omdb_search(Rest).

omdb_search([page=Value|Rest]) -->
	[page=Value],
	omdb_search(Rest).

omdb_search([callback=Value|Rest]) -->
	[callback=Value],
	omdb_search(Rest).

omdb_search([v=Value|Rest]) -->
	[version=Value],
	omdb_search(Rest).
