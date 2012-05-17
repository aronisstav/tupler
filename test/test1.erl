-module(test1).

-export([test_func1/1]).
-export([test_func2/1]).
-export([test_func3/1]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list(),
	  field3 = [] :: list(),
	  field4 = [] :: list()
	 }).

test_func1(Data) ->
	A = Data#state.field1,
	B = test_func2(Data).

test_func2(Data) ->
	C = Data#state.field2,
	D = test_func3(Data).

test_func3(Data) ->
	E = Data#state.field4,
	E.
