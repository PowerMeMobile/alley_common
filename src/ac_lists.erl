-module(ac_lists).

-export([
    remove/2,
    group/1,
    groupwith/2,
    findwith/2,
    permutations/1,
    make_ranges/1,
    make_frequencies/1,
    make_pair/2,
    unzip4/1,
    zip4/4
]).

%-define(TEST, 1).
-ifdef(TEST).
   -include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API
%% ===================================================================

-spec remove(N::integer(), List::[term()]) -> [term()].
remove(_, []) -> [];
remove(1, [_ | T]) -> T;
remove(N, [H | T]) -> [H | remove(N-1, T)].

-spec findwith(fun((A::term()) -> boolean()), [A::term()]) -> {value, A::term()} | false.
findwith(_, []) ->
    false;
findwith(Pred, [H | T]) ->
    case Pred(H) of
        true ->
            {value, H};
        false ->
            findwith(Pred, T)
    end.

-spec group([A]) -> [[A]].
group(List) ->
    groupwith(fun erlang:'=:='/2, List).

-spec groupwith(Eq::fun((A, A) -> boolean()), [A]) -> [[A]].
groupwith(_, []) ->
    [];
groupwith(Eq, [X | XS]) ->
    {YS, ZS} = lists:splitwith(fun(I) -> Eq(X, I) end, XS),
    [[X | YS] | groupwith(Eq, ZS)].

-spec permutations([A]) -> [[A]].
permutations([]) ->
    [[]];
permutations(L) ->
    [[H | T] || H <- L, T <- permutations(L -- [H])].

-spec make_ranges([A]) -> [{A, A}].
make_ranges(List) ->
    make_ranges(List, []).
make_ranges([_ | []], Ranges) ->
    lists:reverse(Ranges);
make_ranges([F, S | T], Ranges) ->
    make_ranges([S | T], [{F, S} | Ranges]).

-spec make_frequencies([A]) -> [{A, pos_integer()}].
make_frequencies(Timestamps) ->
    Groups = group(lists:sort(Timestamps)),
    [{hd(L), length(L)} || L <- Groups].

-spec make_pair(KeyN::integer(), Tuple::tuple()) -> {Key::term(), Value::tuple()} | {Key::term(), Value::term()}.
make_pair(KeyN, Tuple) ->
    Key = element(KeyN, Tuple),
    ValueList = remove(KeyN, tuple_to_list(Tuple)),
    Value = case length(ValueList) of
                1 -> hd(ValueList);
                _ -> list_to_tuple(ValueList)
            end,
    {Key, Value}.

-spec unzip4([{A,B,C,D}]) -> {[A],[B],[C],[D]}.
unzip4(Ts) ->
    lists:foldr(
        fun({A,B,C,D}, {AccA,AccB,AccC,AccD}) ->
            {[A|AccA], [B|AccB], [C|AccC], [D|AccD]}
        end,
        {[],[],[],[]},
        Ts).

-spec zip4([A],[B],[C],[D]) -> [{A,B,C,D}].
zip4(A, B, C, D) ->
    zip4(A, B, C, D, []).

zip4([], [], [], [], Acc) ->
    lists:reverse(Acc);
zip4([A|As], [B|Bs], [C|Cs], [D|Ds], Acc) ->
    zip4(As, Bs, Cs, Ds, [{A,B,C,D} | Acc]).

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

remote_test() ->
    ?assertEqual("acdef", remove(2, "abcdef")),
    ?assertEqual("", remove(10, "")).

findwith_test() ->
    ok.

group_test() ->
    ?assertEqual(["M","i","ss","i","ss","i","pp","i"], group("Mississippi")).

groupwith_test() ->
    ok.

permutations_test() ->
    ?assertEqual([[b,u,g],[b,g,u],[u,b,g],[u,g,b],[g,b,u],[g,u,b]], permutations([b,u,g])).

make_ranges_test() ->
    ?assertEqual([{1,2},{2,3},{3,4},{4,5}], make_ranges([1,2,3,4,5])).

make_frequencies_test() ->
    ?assertEqual([{1,1},{2,2},{3,3}], make_frequencies([1,2,3,2,3,3])).

make_pair_test() ->
    ?assertEqual({b,{a,c}}, make_pair(2, {a,b,c})),
    ?assertEqual({a,b}, make_pair(1, {a,b})).

unzip4_test() ->
    ?assertEqual({[],[],[],[]}, unzip4([])),
    ?assertEqual({[1,2],[1,2],[1,2],[1,2]}, unzip4([{1,1,1,1},{2,2,2,2}])).

zip4_test() ->
    ?assertEqual([], zip4([], [], [], [])),
    ?assertEqual([{1,1,1,1},{2,2,2,2}], zip4([1,2],[1,2],[1,2],[1,2])).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
