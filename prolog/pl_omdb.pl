:- module(pl_omdb, []).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(uri)).
:- use_module(library(yall)).
:- use_module(omdb_types).


omdb_api("http://www.omdbapi.com/?t=~s&y=~s&plot=~s&r=json").
omdb_search("http://www.omdbapi.com/?s=~s&y=~s&page=~s&r=json").
omdb_poster_api("http://img.omdbapi.com/?apikey=~s&").


omdb_dict(Dict, [Title,Year,Plot]) :-
  maplist([X]>>ignore(X=""), [Title,Year,Plot]),
  maplist([X,S]>>( uri_encoded(fragment, X, E), atom_string(E, S) ),
    [Title,Year,Plot], [T,Y,P]),
  Params = [T,Y,P],
  maplist(must_be(omdb_param), Params),
  omdb_api(Template),
  format(string(Request), Template, Params),
  setup_call_cleanup(
    (  http_open(Request, Stream, [timeout(20)]),
       set_stream(Stream, encoding(utf8))
    ),
    json_read_dict(Stream, Dict),
    close(Stream)
  ).

omdb_search(Dict, [Title,Year,Page]) :-
  maplist([X]>>ignore(X=""), [Title,Year,Page]),
  maplist([X,S]>>( uri_encoded(fragment, X, E), atom_string(E, S) ),
    [Title,Year,Page], [T,Y,P]),
  Params = [T,Y,P],
  maplist(must_be(omdb_param), Params),
  omdb_search(Template),
  format(string(Request), Template, Params),
  setup_call_cleanup(
    (  http_open(Request, Stream, [timeout(20)]),
       set_stream(Stream, encoding(utf8))
    ),
    json_read_dict(Stream, Dict),
    close(Stream)
  ).


omdb_fetch(Key=Value, Options) :-
  omdb_dict(Dict, Options),
  omdb_key(Dict, Key, Value).

omdb_key(error{'Error':"Movie not found!",'Response':"False"}, _, _) :-
  throw(movie_not_found).

omdb_key(Dict, KeyType, Value) :-
  when(ground(KeyType), must_be(omdb_key, KeyType)),
  omdb_key(KeyType, Key),
  Value = Dict.Key.
