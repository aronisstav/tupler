-module(record6).

-export([foo/1]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list()
	 }).

-record(state1, {
	  field1 = [] :: list(),
	  field2 = [] :: list()
	 }).

foo(Arg) ->
    case get(god) of
	jaha -> Arg#state.field1;
 	nehe -> Arg#state1.field1;
	jojo -> somebody:does_something(Arg)
    end.
