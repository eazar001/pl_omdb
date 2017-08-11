:- module(omdb_types, []).

:- use_module(library(typedef)).


:- type retrieval_option --->
	id
	;title
	;'type'
	;year
	;plot
	;tomatoes
	;callback
	;version.

:- type search_option --->
	title
	;'type'
	;year
	;page
	;callback
	;version.
