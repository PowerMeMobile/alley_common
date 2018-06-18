-module(ac_dict).

-export([
    prepend/3
]).

%-define(TEST, 1).
-ifdef(TEST).
   -include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API
%% ===================================================================

-spec prepend(any(), any(), dict:dict()) -> dict:dict().
prepend(Key, Value, Dict) ->
    dict:update(Key, fun(Acc) -> [Value | Acc] end, [Value], Dict).

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

prepend_test() ->
  D0 = dict:new(),
  D1 = dict:store(files, [], D0),
  D2 = ac_dict:prepend(files, f1, D1),
  D3 = ac_dict:prepend(files, f2, D2),
  D4 = ac_dict:prepend(files, f3, D3),
  ?assertEqual([f3,f2,f1], dict:fetch(files, D4)).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
