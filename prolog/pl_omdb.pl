:- module(pl_omdb, [
	 omdb_fetch/3,
	 omdb_search/3,
	 omdb_search_results/4,
	 omdb_fetch_dict/3,
	 omdb_search_dict/3
]).

:- use_module(library(lists), [member/2]).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
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


%! omdb_fetch(+ApiKey, ?KVPair, +Options) is nondet.
%
%  True if Options is a supplied list of API parameters that fetches a valid
%  result from the OMDB API that corresponds to a set of Key=Value pairs
%  represented by KVPair.
omdb_fetch(ApiKey, Key=Value, Options) :-
	omdb_call(retrieval, ApiKey, Dict, Options),
	Value = Dict.Key.

%! omdb_search(+ApiKey, ?KVPair, +Options) is nondet.
%
%  True if Options is a supplied list of API paremters that fetches a valid
%  OMDB object which contains the number of search results and a list of OMDB
%  dictionaries which each represents a search result. Both the list of search
%  results and the number of results are part of KVPair (Key=Value).
omdb_search(ApiKey, Key=Value, Options) :-
	omdb_call(search, ApiKey, Dict, Options),
	Value = Dict.Key.

%! omdb_search_results(+ApiKey, ?KVPair, +Options, ?NumResults) is nondet.
%
%  Like omdb_search/3, except all the Key=Value pairs are iterated through
%  automatically without needed to do any further unwrapping. NumResults is
%  the number of search results found by the search query.
omdb_search_results(ApiKey, Key=Value, Options, NumResults) :-
	omdb_search_dict(ApiKey, Dict, Options),
	NumResults = Dict.'totalResults',
	SearchResults = Dict.'Search',
	member(OneResult, SearchResults),
	Value = OneResult.Key.

%! omdb_fetch_dict(+ApiKey, -Dict, +Options) is det.
%
%  Like omdb_fetch/3, except the Dict unifies directly with the dictionary object
%  rather than backtracking over individual Key=Value pairs.
omdb_fetch_dict(ApiKey, Dict, Options) :-
	omdb_call(retrieval, ApiKey, Dict, Options).

%! omdb_search_dict(+ApiKey, -Dict, +Options) is det.
%
%  Like omdb_fetch_dict/3 but for search queries.
omdb_search_dict(ApiKey, Dict, Options) :-
	omdb_call(search, ApiKey, Dict, Options).


%--------------------------------------------------------------------------------%
% Internal Predicates
%--------------------------------------------------------------------------------%


omdb_call(retrieval, ApiKey, Dict, Options) :-
	retrieval_query(Options, Template),
	make_connection(ApiKey, Template, Dict).

omdb_call(search, ApiKey, Dict, Options) :-
	search_query(Options, Template),
	make_connection(ApiKey, Template, Dict).

make_connection(ApiKey, Template, Dict) :-
	omdb_api(API),
	format(string(Request0), API, [Template]),
	format(string(Request), "~s&apikey=~s", [Request0, ApiKey]),
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

:- use_module(library(aggregate), [aggregate_all/3]).

get_key(Key) :-
	setup_call_cleanup(
		open('test_files/key.txt', read, Stream),
		read(Stream, Key),
		close(Stream)
	).

test(fetch_one_value) :-
	get_key(Key),
	aggregate_all(
		count,
		omdb_fetch(Key, 'Released'=_Value, [title="Casino Royale",year="2006"]),
		1
	).

test(throw_error) :-
	get_key(Key),
	catch(
		omdb_fetch(Key, 'Released'=_Value, [title="Casino Royale",year="200346"]),
		Error,
		Error=error(
			existence_error(
				key,
				'Released',
				_{'Error':"Movie not found!", 'Response':"False"}
				),
				_
		)
	).

test(search_title) :-
	get_key(Key),
	aggregate_all(
		count,
		omdb_search_results(
			Key,
			'Title'=_Value,
			[title="The Road to Casino Royale"],
			_NumResults
		),
		1
	).

:- end_tests(pl_omdb).
