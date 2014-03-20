-module(ac_utils).

-export([
    safe_foreach/4
]).

-type success() :: any().
-type failure() :: any().
-type match_pattern() :: atom() | tuple().
-type success_pattern() :: match_pattern().
-type failure_pattern() :: match_pattern().
-spec safe_foreach(
    fun((A) -> success() | failure()),
    [A],
    success_pattern() | [success_pattern()],
    failure_pattern() | [failure_pattern()]
) -> ok | failure().

-ifdef(TEST).
   -include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API
%% ===================================================================

safe_foreach(_, [], _, _) ->
    ok;
safe_foreach(Fun, [H|T], SuccessPattern, FailurePattern) ->
    Result = Fun(H),
    case match_pattern(Result, SuccessPattern) of
        true ->
            safe_foreach(Fun, T, SuccessPattern, FailurePattern);
        false ->
            case match_pattern(Result, FailurePattern) of
                true ->
                    Result;
                false ->
                    exit({no_pattern_for, Result})
            end
    end.

%% ===================================================================
%% Internal
%% ===================================================================

-spec match_pattern(any(), match_pattern() | [match_pattern()])  -> boolean().
match_pattern(Expression, Patterns) when is_list(Patterns) ->
    lists:any(fun(Value) -> Value =:= true end,
        [match_pattern(Expression, Pattern) || Pattern <- Patterns]);

match_pattern(Expression, Pattern) ->
    MatchSpec = [{Pattern, [], [true]}],
    CompiledMatchSpec = ets:match_spec_compile(MatchSpec),
    ets:match_spec_run([Expression], CompiledMatchSpec) =:= [true].

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

process_item(Item) when is_integer(Item) ->
 {integer, Item};
process_item(Item) when is_list(Item) ->
 {list, Item};
process_item(Item) ->
 {error, {unknown, Item}}.

safe_foreach_ok_test() ->
    ?assertEqual(ok, safe_foreach(fun process_item/1,
        [1, "2", 3], [{integer, '_'}, {list, '_'}], {error, '_'})).

safe_foreach_error_test() ->
    ?assertEqual({error, {unknown,a}}, safe_foreach(fun process_item/1,
        [1, a, "2", 3, b], [{integer, '_'}, {list, '_'}], {error, '_'})).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
