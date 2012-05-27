-module(record20).

-export([foo/1]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list()
	 }).

foo(Arg) when Arg#state.field1 =:= foo ->
    ok.
