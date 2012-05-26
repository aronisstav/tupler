%% -*- erlang-indent-level: 2 -*-

-module(tupler).

-export([analyze/1]).

-record(state, {
	  warnings       = []            :: list(),
	  module                         :: atom(),
	  records        = dict:new()    :: dict(),
	  function                       :: [{atom(), integer()}],
	  parameter_info = single_clause :: 'single_clause' | list(),
	  var_dict       = dict:new()    :: dict
	 }).

-record(var, {
	  first     = []    :: list(),
	  escapes   = false :: boolean(),
	  as_record = none  :: none | {one, atom(), [atom()]} | multiple
	 }).

%% -define(DEBUG, on).
-ifdef(DEBUG).
-define(debug(A, B), io:format(A, B)).
-else.
-define(debug(A, B), ok).
-endif.
-define(debug(A), ?debug(A, [])).

%% -define(SKIP, true).
-ifdef(SKIP).
-define(skip(Reason), begin ?debug(Reason), halt(1) end).
-else.
-define(skip(Reason), ?debug(Reason)).
-endif.

-spec analyze(file:filename()) -> [term()].

analyze(Filename) ->
  case dialyzer_utils:get_abstract_code_from_beam(Filename) of
    {ok, Abstract} ->
      Warnings = analyze_abstract(Abstract),
      print_warnings(Warnings);
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
      ?debug("\n\nL~w: ~w/~w\n",[_Line, Name, Arity]),
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
	  FieldsSet = ordsets:from_list(Fields),
	  NewRecords = dict:store(Name, FieldsSet, State#state.records),
	  analyze_abstract(Rest, State#state{records = NewRecords});
	_Other ->
	  ?debug("Ignore attribute ~w: ~w\n",[_Other, Form]),
	  analyze_abstract(Rest, State)
      end;
    _Other ->
      ?debug("Ignore ~w: ~w\n",[_Other, Form]),
      analyze_abstract(Rest, State)
  end.

analyze_function_clauses(Name, Arity, [Clause], State) ->
  OldFunction = State#state.function,
  NewState =
    State#state{function = [{Name,Arity}|OldFunction],
		parameter_info = single_clause},
  NewState1 = analyze_single_clause(Clause, NewState),
  NewState1#state{function = OldFunction};
analyze_function_clauses(Name, Arity, Clauses, State) ->
  OldFunction = State#state.function,
  ParameterInfo = lists:duplicate(Arity, []),
  NewState =
    State#state{function = [{Name,Arity}|OldFunction],
		parameter_info = ParameterInfo},
  NewState1 = analyze_function_clauses(Clauses, NewState),
  NewState1#state{function = OldFunction}.

analyze_function_clauses([], State) ->
  State;
analyze_function_clauses([Clause|Rest], State) ->
  ?debug("Clause:\n"),
  NewState = analyze_single_clause(Clause, State),
  analyze_function_clauses(Rest, NewState).
  
analyze_single_clause(Clause, State = #state{parameter_info = single_clause}) ->
  Patterns = erl_syntax:clause_patterns(Clause),
  Guard    = erl_syntax:clause_guard(Clause),
  Body     = erl_syntax:clause_body(Clause),
  ?debug("Patterns:\n~p\n",[Patterns]),
  ?debug("Guard:\n~p\n",[Guard]),
  case is_simple_clause(Patterns, Guard) of
    {true, Vars} ->
      ?debug("VARS:\n~p\n",[Vars]),
      Fold =
	fun({Name, Pos}, Dict) ->
	    dict:store(Name, #var{first = [Pos]}, Dict)
	end,
      OldVarDict = State#state.var_dict,
      VarDict = lists:foldl(Fold, mark_unused(OldVarDict), Vars),
      NewState0 = traverse(Body, State#state{var_dict = VarDict}),
      NewState1 = check_clause(NewState0),
      NewState1#state{var_dict = remove_defs(Vars, NewState1#state.var_dict)};
    false ->
      ?skip("Not simple clause.\n"),
      State
  end;
analyze_single_clause(_Clause, State) ->
  State.

is_simple_clause(Patterns, none) ->
  Pred = fun(Term) ->
	     Type = erl_syntax:type(Term),
	     var_or_underscore(Type)
	 end,
  {Vars, NonVars} = lists:partition(Pred, Patterns),
  case NonVars =:= [] of
    true  ->
      {true, [{erl_syntax:variable_name(Var), erl_syntax:get_pos(Var)} ||
	       Var <- Vars]};
    false -> false
  end.

var_or_underscore(variable) -> true;
var_or_underscore(underscore) -> true;
var_or_underscore(_Other) -> false.

mark_unused(Dict) ->
  Map = fun(_Key, Val) -> #var{first = Val#var.first} end,
  dict:map(Map, Dict).

traverse([Stmt|Rest], #state{var_dict = VarDict} = State) ->
  ?debug("Stmt:\n~p\n",[Stmt]),
  Pos = erl_syntax:get_pos(Stmt),
  NewState =
    case erl_syntax:type(Stmt) of
      record_access ->
	Arg = erl_syntax:variable_name(erl_syntax:record_access_argument(Stmt)),
	Field = erl_syntax:concrete(erl_syntax:record_access_field(Stmt)),
	Record = erl_syntax:data(erl_syntax:record_access_type(Stmt)),
	?debug("Arg: ~p\nField: ~p\nType: ~p\n",
	       [Arg, Field, Record]),
	NewVar =
	  case dict:find(Arg, VarDict) of
	    {ok, Value} ->
	      ?debug("Lookup value: ~p\n", [Value]),
	      case Value#var.as_record of
		none ->
		  Value#var{as_record = {Record, [Field]}};
		{Record, Fields} ->
		  NewFields = ordsets:add_element(Field, Fields),
		  Value#var{as_record = {Record, NewFields}};
		_Other ->
		  Value#var{as_record = multiple}
	      end;
	    error ->
	      ?debug("Lookup error\n"),
	      #var{as_record = {Record, [Field]}}
	  end,
	State#state{var_dict = dict:store(Arg, NewVar, VarDict)};
      match_expr ->
	Pattern = erl_syntax:match_expr_pattern(Stmt),
	Body = erl_syntax:match_expr_body(Stmt),
	?debug("Pattern:~p\nBody:~p\n",[Pattern, Body]),
	traverse([Pattern,Body], State);
      variable ->
	Name = erl_syntax:variable_name(Stmt),
	case dict:find(Name, VarDict) of
	  {ok, _Value} -> State;
	  error ->
	    NewVarDict =
	      dict:store(Name, #var{first = [Pos]}, State#state.var_dict),
	    State#state{var_dict = NewVarDict}
	end;
      fun_expr ->
	Clauses = erl_syntax:fun_expr_clauses(Stmt),
	Arity = erl_syntax:fun_expr_arity(Stmt),
	analyze_function_clauses({anon, Pos}, Arity, Clauses, State);
      _Other ->
	?debug("Type: ~p\n",[_Other]),
	State
    end,
  traverse(Rest, NewState);
traverse([], State) ->
  State.
  
remove_defs(Vars, Dict) ->
  Fold = fun(Key, InDict) -> dict:erase(Key, InDict) end,
  lists:foldl(Fold, Dict, Vars).

check_clause(#state{var_dict = VarDict, records = Records,
		    function = [Fun|_]} = State) ->
  Fold =
    fun(Key, Value, Acc) ->
	case Value#var.as_record of
	  none -> Acc;
	  multiple -> Acc;
	  {Record, Fields} ->
	    AllFields = dict:fetch(Record, Records),
	    case ordsets:subtract(AllFields, Fields) of
	      [] -> Acc;
	      Missing ->
		?debug("MISSING!\nVar:~p\nFields:~p\n",[Value, Missing]),
		[{Fun, Key, Value#var.first, Record, Missing}|Acc]
	    end
	end
    end,
  Warns = dict:fold(Fold, [], VarDict),
  State#state{warnings = Warns ++ State#state.warnings}.
		  
print_warnings([]) ->
  ok;
print_warnings([{Fun, Var, [Loc], Record, Missing}|Rest]) ->
  {Name, Arity} = Fun,
  Id =
    case Name of
      {anon, Line} ->
	io_lib:format("an anonymous function defined at line ~p",[Line]);
      Other        ->
	io_lib:format("~p/~p",[Other, Arity])
    end,
  io:format("~p: Within the scope of ~s, the variable ~p is being used only as a"
	    " value of record #~p but the fields ~w are never being used.\n",
	    [Loc, Id, Var, Record, Missing]),
  print_warnings(Rest).
  
