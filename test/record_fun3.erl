-module(record_fun3).

-export([foo/1]).

-record(state,{
	  field1 :: term(),
	  field2 :: term(),
	  field3 :: term(),
	  field4 :: term()
	 }).

foo(Parameter) ->
    fun() ->
	    Parameter#state.field1
    end.
	  
