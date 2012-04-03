-module(example1).

-export([foo/1]).

foo(Arg) ->
    {Useful, _} = Arg,
    Useful.
