:- module(omdb_types, [omdb_key/2]).

:- use_module(library(typedef)).

:- type omdb_param == string.

:- type searchtype ---> movie ; series ; episode.

:- type omdb_key --->
      media_type
     ;title
     ;year
     ;plot
     ;rated
     ;released
     ;runtime
     ;genre
     ;director
     ;writer
     ;actors
     ;language
     ;country
     ;awards
     ;poster
     ;metascore
     ;imdb_rating
     ;imdb_votes
     ;imdb_id
     ;response.

omdb_key(media_type, 'Type').
omdb_key(title, 'Title').
omdb_key(year, 'Year').
omdb_key(plot, 'Plot').
omdb_key(rated, 'Rated').
omdb_key(released, 'Released').
omdb_key(runtime, 'Runtime').
omdb_key(genre, 'Genre').
omdb_key(director, 'Director').
omdb_key(writer, 'Writer').
omdb_key(actors, 'Actors').
omdb_key(language, 'Language').
omdb_key(country, 'Country').
omdb_key(awards, 'Awards').
omdb_key(poster, 'Poster').
omdb_key(metascore, 'Metascore').
omdb_key(imdb_rating, 'imdbRating').
omdb_key(imdb_votes, 'imdbVotes').
omdb_key(imdb_id, 'imdbID').
omdb_key(response, 'Response').
