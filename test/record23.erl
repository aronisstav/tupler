-module(record23).

-export([foo/1]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list()
	 }).

foo(Arg) ->
    God = get(god),
    if
	God -> Arg#state.field1;
	true -> ok
    end.
