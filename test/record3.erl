-module(record3).

-export([foo/2]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list(),
	  field3 = [] :: list()
	 }).

foo(1, Arg) ->
    Arg#state.field1;
foo(2, Other) ->
    Other#state.field2.
