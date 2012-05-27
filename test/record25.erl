-module(record25).

-export([foo/1]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list()
	 }).

foo(Arg) ->
    NewField1 = someone:update(Arg#state.field1),
    Arg#state{field1 = NewField1}.
    
