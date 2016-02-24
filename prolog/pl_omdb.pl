:- module(pl_omdb, []).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(uri)).
:- use_module(library(yall)).
:- use_module(omdb_types).
:- use_module(omdb_query).


omdb_api("http://www.omdbapi.com/?~s&r=json").
omdb_poster_api("http://img.omdbapi.com/?~s&apikey=~s&").


omdb_fetch(Key=Value, Options) :-
  omdb_call(retrieval, Dict, Options),
  omdb_key(Dict, Key, Value).

omdb_search(Key=Value, Options) :-
  omdb_call(search, Dict, Options),
  omdb_key(Dict, Key, Value).


omdb_call(retrieval, Dict, Options) :-
  retrieval_query(Options, Template),
  omdb_api(API),
  format(string(Request), API, [Template]),
  omdb_connect(Request, Dict).


omdb_call(search, Dict, Options) :-
  search_query(Options, Template),
  omdb_api(API),
  format(string(Request), API, [Template]),
  omdb_connect(Request, Dict).


omdb_key(error{'Error':"Movie not found!",'Response':"False"}, _, _) :-
  throw(movie_not_found).

omdb_key(Dict, KeyType, Value) :-
  when(ground(KeyType), must_be(omdb_key, KeyType)),
  omdb_key(KeyType, Key),
  Value = Dict.Key.


%--------------------------------------------------------------------------------%
% Internal Predicates
%--------------------------------------------------------------------------------%


omdb_connect(Request, Dict) :-
  setup_call_cleanup(
    (  http_open(Request, Stream, [timeout(20)]),
       set_stream(Stream, encoding(utf8))
    ),
    json_read_dict(Stream, Dict),
    close(Stream)
  ).
