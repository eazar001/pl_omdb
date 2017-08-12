pl-omdb
=======
This is A SWI-Prolog interface to the OMDB (Open Movie Database) API http://www.omdbapi.com

Examples of Usage
=================
```prolog

?- use_module(library(pl_omdb)).

% This will fetch the release date for Casino Royale, year 2006.
?- omdb_fetch(your_api_key, 'Released'=Value, [title="Casino Royale",year="2006"]).

% This will fetch the object that corresponds to Casino Royale, 2006, with all the keys
%  and respective values ... including results specific to Rotten Tomatoes.
?- omdb_fetch(your_api_key, Key=Value, [title="Casino Royale",year="2006",tomatoes="true"]).

% Return list of search results along with number of results.
?- omdb_search(your_api_key, Key=Value, [title="The Matrix"]).

% Iterate through all the search results while also unifying with the number of results found.
?- omdb_search_results(your_api_key, Key=Value, [title="The Matrix"], NumResults).

% Fetch the dictionary object for all titles matching "The Matrix".
?- omdb_fetch_dict(your_api_key, Dict, [title="The Matrix"]).

% Fetch the dictionary object for search results pertaining to the title "The Matrix".
?- omdb_search_dict(your_api_key, Dict, [title="The Matrix"]).
```

The `Options` list is a list of valid parameters to pass to the OMDB API. All parameters are
essentially key values that are of the type atom (described below). All of these keys correspond
to a value (all represented as string types).

Here are some valid options for fetches/retrievals:
* id
* title
* 'type'
* year
* plot
* tomatoes
* callback
* version

... And for searches:
* title
* 'type'
* year
* page
* callback
* version

For more information regarding the parameters visit the official website link posted at the top of
this page.
