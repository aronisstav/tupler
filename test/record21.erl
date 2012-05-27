-module(record21).

-export([foo/1]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list()
	 }).

foo(Arg) ->
    case get(god) of
	jaha -> Arg#state.field1;
	nehe -> Arg#state.field2
    end.
