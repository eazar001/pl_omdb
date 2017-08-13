:- module(omdb_types, []).

:- use_module(library(typedef)).


:- type retrieval_option --->
	id
	;title
	;media_type
	;year
	;plot
	;tomatoes
	;callback
	;version.

:- type search_option --->
	title
	;media_type
	;year
	;page
	;callback
	;version.
