%% -*- erlang-indent-level: 2 -*-

-module(tupler).

-export([analyze/1]).

-spec analyze(file:filename()) -> [term()].

analyze(Filename) ->
  case dialyzer_utils:get_abstract_code_from_beam(Filename) of
    {ok, Abstract} ->
      analyze_abstract(Abstract);
    error -> 
      io:format("Unable to retrieve abstract code from ~s\n",[Filename])
  end.

analyze_abstract(Abstract) ->
  analyze_abstract(Abstract, []).

analyze_abstract([], Warnings) ->
  Warnings;
analyze_abstract([{function, Line, Name, Arity, Clauses}|Rest], Warnings) ->
  io:format("L~w: ~w/~w\n\t~p\n",[Line, Name, Arity, Clauses]),
  analyze_abstract(Rest, Warnings);
analyze_abstract([Ignore|Rest], Warnings) ->
  io:format("Ignore: ~w\n",[Ignore]),
  analyze_abstract(Rest, Warnings).

  

