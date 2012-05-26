-module(record11).

-export([foo/0]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list()
	 }).

foo() ->
    A = #state{field1 = foo, field2 = bar},
    A#state.field1.
