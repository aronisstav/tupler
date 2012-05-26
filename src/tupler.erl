%% -*- erlang-indent-level: 2 -*-

-module(tupler).

-export([analyze/1]).

-record(state, {
	  warnings       = []         :: list(),
	  module                      :: atom(),
	  function_name               :: atom(),
	  function_arity              :: integer(),
	  parameter_info = []         :: list(),
	  records        = dict:new() :: dict()
	 }).

%% -define(DEBUG, on).
-ifdef(DEBUG).
-define(debug(A, B), io:format(A, B)).
-else.
-define(debug(A, B), ok).
-endif.
-define(debug(A), ?debug(A, [])).

-spec analyze(file:filename()) -> [term()].

analyze(Filename) ->
  case dialyzer_utils:get_abstract_code_from_beam(Filename) of
    {ok, Abstract} ->
      analyze_abstract(Abstract);
    error -> 
      io:format("Unable to retrieve abstract code from ~s\n",[Filename])
  end.

analyze_abstract(Abstract) ->
  analyze_abstract(Abstract, #state{}).

analyze_abstract([], #state{warnings = Warnings}) ->
  Warnings;
analyze_abstract([Form|Rest], State) ->
  case erl_syntax:type(Form) of
    function ->
      _Line    = erl_syntax:get_pos(Form),
      Name    = erl_syntax:concrete(erl_syntax:function_name(Form)),
      Arity   = erl_syntax:function_arity(Form),
      Clauses = erl_syntax:function_clauses(Form),
      ?debug("L~w: ~w/~w\n",[_Line, Name, Arity]),
      NewState = analyze_function_clauses(Name, Arity, Clauses, State),
      analyze_abstract(Rest, NewState);
    attribute ->
      case erl_syntax:concrete(erl_syntax:attribute_name(Form)) of
	module ->
	  [Module] = [erl_syntax:concrete(Arg) ||
		       Arg <- erl_syntax:attribute_arguments(Form)],
	  ?debug("Module: ~w\n",[Module]),
	  analyze_abstract(Rest, State#state{module = Module});
	record ->
	  [TName, TFields] = erl_syntax:attribute_arguments(Form),
	  Name = erl_syntax:data(TName),
	  Fields =
	    [erl_syntax:concrete(FName) ||
	      FName <-
		[erl_syntax:record_field_name(TTField) ||
		  TTField <- erl_syntax:tuple_elements(TFields)
		]
	    ],
	  ?debug("Record: ~p, ~p\n",[Name, Fields]),
	  NewState =
	    State#state{records =
			  dict:store(Name, Fields, State#state.records)},
	  analyze_abstract(Rest, NewState);
	_Other ->
	  ?debug("Ignore attribute ~w: ~w\n",[_Other, Form]),
	  analyze_abstract(Rest, State)
      end;
    _Other ->
      ?debug("Ignore ~w: ~w\n",[_Other, Form]),
      analyze_abstract(Rest, State)
  end.

analyze_function_clauses(Name, Arity, Clauses, State) ->
  ParameterInfo = lists:duplicate(Arity, []),
  NewState =
    State#state{function_name  = Name, function_arity = Arity,
		parameter_info = ParameterInfo},
  analyze_function_clauses(Clauses, NewState).

analyze_function_clauses([], State) ->
  State;
analyze_function_clauses([Clause|Rest], State) ->
  ?debug("Clause:\n"),
  Patterns = erl_syntax:clause_patterns(Clause),
  Guard    = erl_syntax:clause_guard(Clause),
  Body     = erl_syntax:clause_body(Clause),
  ?debug("Patterns:\n~p\n",[Patterns]),
  ?debug("Guard:\n~p\n",[Guard]),
  ?debug("Body:\n~p\n",[Body]),
  
  analyze_function_clauses(Rest, State).
  


