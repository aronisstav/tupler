-module(record15).

-export([foo/1]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list()
	 }).

foo(Arg) ->
    ok = somebody:checks_something(Arg#state.field1),
    Arg.
