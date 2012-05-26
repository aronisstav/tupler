-module(record7).

-export([foo/1]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list()
	 }).

foo(Arg) ->
    Arg#state.field1,
    Arg#state.field2.
