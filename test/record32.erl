-module(record32).

-export([foo/1]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list()
	 }).

foo(Arg) ->
    Field = Arg#state.field2,
    receive
	Arg -> {ok, Field}
    end.
