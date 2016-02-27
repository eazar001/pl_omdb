:- module(pl_omdb,
     [ omdb_fetch/2
      ,omdb_search/2
      ,omdb_fetch_dict/2
      ,omdb_search_dict/2 ]).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(uri)).
:- use_module(omdb_types).
:- use_module(omdb_query).


omdb_api("http://www.omdbapi.com/?~s&r=json").
omdb_poster_api("http://img.omdbapi.com/?~s&apikey=~s&").


omdb_fetch(Key=Value, Options) :-
  omdb_call(retrieval, Dict, Options),
  Value = Dict.Key.

omdb_search(Key=Value, Options) :-
  omdb_call(search, Dict, Options),
  Value = Dict.Key.

omdb_fetch_dict(Dict, Options) :-
  omdb_call(retrieval, Dict, Options).

omdb_search_dict(Dict, Options) :-
  omdb_call(search, Dict, Options).


omdb_call(Call, Dict, Options) :-
  (  Call = retrieval,
     retrieval_query(Options, Template)
  ;  Call = search,
     search_query(Options, Template)
  ),
  omdb_api(API),
  format(string(Request), API, [Template]),
  omdb_connect(Request, Dict).


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
