pl-omdb
=======
This is A SWI-Prolog interface to the OMDB (Open Movie Database) API http://www.omdbapi.com

Examples of Usage
=================
```prolog

% Loading pl_omdb will create a special API key flag to store the API key to be referenced later
?- use_module(library(pl_omdb)).

% The user can store the API key in the special flag for access at runtime now
?- set_prolog_flag(omdb_api_key, 'my_unique_key')

% This will fetch the release date for Casino Royale, year 2006.
?- omdb_fetch('Released'=Value, [title='Casino Royale',year='2006']).

% This will fetch the object that corresponds to Casino Royale, 2006, with all the keys
%  and respective values ... including results specific to Rotten Tomatoes.
?- omdb_fetch(Key=Value, [title='Casino Royale',year='2006',tomatoes='true']).

% Return list of search results along with number of results.
?- omdb_search(Key=Value, [title='The Matrix']).

% Iterate through all the search results while also unifying with the number of results found.
?- omdb_search_results(Key=Value, [title='The Matrix'], NumResults).

% Fetch the dictionary object for all titles matching "The Matrix".
?- omdb_fetch_dict(Dict, [title='The Matrix']).

% Fetch the dictionary object for search results pertaining to the title "The Matrix".
?- omdb_search_dict(Dict, [title='The Matrix']).
```

Alternatively, one may specify the API key in the above examples as a first argument, if they
wish to bypass the flag mechanism and/or use multiple keys in one application.

The `Options` list is a list of valid parameters to pass to the OMDB API. All parameters are
essentially key values that are of the type atom (described below). All of these keys correspond
to a value (all represented as string types).

Here are some valid options for fetches/retrievals:
* id
* title
* media_type (this is the OMDB 'type' parameter)
* year
* plot
* tomatoes
* callback
* version

... And for searches:
* title
* media_type (this is the OMDB 'type' parameter)
* year
* page
* callback
* version

For more information regarding the parameters visit the official website link posted at the top of
this page.
