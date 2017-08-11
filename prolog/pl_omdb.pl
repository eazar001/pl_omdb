:- module(pl_omdb, [
	 omdb_fetch/2,
	 omdb_search/2,
	 omdb_search_results/3,
	 omdb_fetch_dict/2,
	 omdb_search_dict/2
]).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(uri)).
:- use_module(omdb_types).
:- use_module(omdb_query).


/** <module> pl_omdb API
This module implements a convenience layer over the OMDB API located at:
http://www.omdbapi.com

The author of this convenience layer module is not the author of the main API nor
affiliated with the official API/website itself.

@author Ebrahim Azarisooreh
@license MIT
*/


omdb_api("http://www.omdbapi.com/?~s&r=json").
omdb_poster_api("http://img.omdbapi.com/?~s&apikey=~s&").


%% omdb_fetch(?KVPair, +Options) is nondet.
%
%  True if Options is a supplied list of API parameters that fetches a valid
%  result from the OMDB API that corresponds to a set of Key=Value pairs
%  represented by KVPair.

omdb_fetch(Key=Value, Options) :-
	omdb_call(retrieval, Dict, Options),
	Value = Dict.Key.


%% omdb_search(?KVPair, +Options) is nondet.
%
%  True if Options is a supplied list of API paremters that fetches a valid
%  OMDB object which contains the number of search results and a list of OMDB
%  dictionaries which each represents a search result. Both the list of search
%  results and the number of results are part of KVPair (Key=Value).

omdb_search(Key=Value, Options) :-
	omdb_call(search, Dict, Options),
	Value = Dict.Key.


%% omdb_search_results(?KVPair, +Options, ?NumResults) is nondet.
%
%  Like omdb_search/2, except all the Key=Value pairs are iterated through
%  automatically without needed to do any further unwrapping. NumResults is
%  the number of search results found by the search query.

omdb_search_results(Key=Value, Options, NumResults) :-
	omdb_search_dict(Dict, Options),
	NumResults = Dict.'totalResults',
	SearchResults = Dict.'Search',
	member(OneResult, SearchResults),
	Value = OneResult.Key.


%% omdb_fetch_dict(-Dict, +Options) is det.
%
%  Like omdb_fetch/2, except the Dict unifies directly with the dictionary object
%  rather than backtracking over individual Key=Value pairs.

omdb_fetch_dict(Dict, Options) :-
	omdb_call(retrieval, Dict, Options).


%% omdb_search_dict(-Dict, +Options) is det.
%
%  Like omdb_fetch_dict/2 but for search queries.
omdb_search_dict(Dict, Options) :-
	omdb_call(search, Dict, Options).


%--------------------------------------------------------------------------------%
% Internal Predicates
%--------------------------------------------------------------------------------%


omdb_call(Call, Dict, Options) :-
	(	Call = retrieval,
		retrieval_query(Options, Template)
	;	Call = search,
		search_query(Options, Template)
	),
	omdb_api(API),
	format(string(Request), API, [Template]),
	omdb_connect(Request, Dict).


omdb_connect(Request, Dict) :-
	setup_call_cleanup(
		(	http_open(Request, Stream, [timeout(20)]),
	   		set_stream(Stream, encoding(utf8))
		),
		json_read_dict(Stream, Dict),
		close(Stream)
	).

:- begin_tests(pl_omdb).

test(fetch_one_value) :-
	aggregate_all(
		count,
		omdb_fetch('Released'=_Value, [title="Casino Royale",year="2006"]),
		1
	).

test(throw_error) :-
	catch(omdb_fetch('Released'=_Value, [title="Casino Royale",year="200346"]),
	  Error,
	  Error=error(
	  existence_error(
		key,
		'Released',
		_{'Error':"Movie not found!", 'Response':"False"}), _)
	).

test(search_title) :-
	aggregate_all(
		count,
		omdb_search_results(
			'Title'=_Value,
			[title="The Road to Casino Royale"],
			_NumResults
		),
		1
	).

:- end_tests(pl_omdb).
