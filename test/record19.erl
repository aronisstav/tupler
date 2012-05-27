-module(record19).

-export([foo/1]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list()
	 }).

foo(Arg) ->
    case get(god) of
	A when A =:= Arg#state.field1 -> ok;
	other -> false
    end.
