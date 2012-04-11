-module(tuple_fun1).

-export([foo/1]).

foo(Arg) ->
    A =
	fun() ->
		{Useful, _} = Arg,
		Useful
	end,
    A().
