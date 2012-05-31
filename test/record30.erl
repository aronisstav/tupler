-module(record30).

-export([foo/1]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list()
	 }).

foo(Arg) ->
    case get(god) of
	{ok, Arg} -> Arg#state.field2
    end.
