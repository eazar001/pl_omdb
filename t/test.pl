:- use_module(library(omdb)).
:- use_module(library(tap)).

fetch_one_value :-
  aggregate_all(
    count,
    omdb_fetch('Released'=_Value, [title="Casino Royale",year="2006"]),
    1
  ).

throw_error :-
  catch(omdb_fetch('Released'=_Value, [title="Casino Royale",year="200346"]),
    Error,
    Error=error(
      existence_error(
        key,
        'Released',
        _{'Error':"Movie not found!", 'Response':"False"}), _)
  ).

search_title :-
  aggregate_all(count,
    omdb_search_results(
      'Title'=_Value,
      [title="The Road to Casino Royale"],
       _NumResults),
    1
    ).
