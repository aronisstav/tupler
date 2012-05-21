%%% See also record_fun1.erl. In this case the state record is used in the fun
%%% and we shouldn't emit a warning.

-module(record_fun2).

-export([foo/0]).

-record(state,{
	  field1 :: term(),
	  field2 :: term(),
	  field3 :: term(),
	  field4 :: term()
	 }).

foo() ->
    Parameter =
	#state{field1 = val1,
	       field2 = val2,
	       field3 = val3,
	       field4 = val4},
    Fun =
	fun() ->
		Info1 = Parameter#state.field1,
		bar(Info1, Parameter)
	end,
    Fun().
	  
bar(Field1, State) ->
    somebody:does_something(Field1, State).
