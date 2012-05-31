-module(record31).

-export([foo/1]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list()
	 }).

foo(Arg) ->
    receive
	ok -> Arg#state.field2
    end.
