-module(record8).

-export([foo/1]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list(),
	  field3 = [] :: list(),
	  field4 = [] :: list(),
	  field5 = [] :: list(),
	  field6 = [] :: list(),
	  field7 = [] :: list()
	 }).

foo(Arg) ->
    Arg#state.field5,
    Arg#state.field1.
