%% Based on a false warning from OTP's dict library.

-module(record29).

-export([get_slot/2]).

-record(state, {
	  field1 = [] :: list(),
	  field2 = [] :: list(),
	  field3 = [] :: list(),
	  field4 = [] :: list(),
	  field5 = [] :: list()
	 }).

get_slot(T, Key) ->
    H = erlang:phash(Key, T#state.field1),
    if
	H > T#state.field2 -> H - T#state.field3;
	true -> H
    end.
