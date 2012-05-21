%%% This test is very close to the real problem we are seeking to detect and
%%% warn about: A (possibly huge) state record is passed in the environment of a
%%% fun, only to access a very limited field.

-module(record_fun1).

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
		Parameter#state.field1
	end,
    Fun().
	  
