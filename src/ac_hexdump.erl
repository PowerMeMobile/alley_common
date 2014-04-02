-module(ac_hexdump).

-export([
    hexdump_to_list/1,
    hexdump_to_binary/1,
    list_to_hexdump/2,
    binary_to_hexdump/2
]).

-ifdef(TEST).
   -include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API
%% ===================================================================

-spec hexdump_to_list(string()) -> list().
hexdump_to_list(HexStr) ->
    hexdump_to_list(HexStr, []).

-spec hexdump_to_binary(binary()) -> binary().
hexdump_to_binary(HexBin) ->
    list_to_binary(hexdump_to_list(binary_to_list(HexBin))).

-spec list_to_hexdump(list(), to_upper | to_lower) -> string().
list_to_hexdump(List, Case) ->
    list_to_hexdump(List, Case, []).

-spec binary_to_hexdump(binary(), to_upper | to_lower) -> binary().
binary_to_hexdump(Bin, Case) ->
    list_to_binary(list_to_hexdump(binary_to_list(Bin), Case)).

%% ===================================================================
%% Internal
%% ===================================================================

hexdump_to_list([], Acc) ->
    lists:reverse(Acc);
hexdump_to_list(HexDump, Acc) ->
    {Hex, Rest} = lists:split(2, HexDump),
    {ok, [V], []} = io_lib:fread("~16u", Hex),
    hexdump_to_list(Rest, [V | Acc]).

list_to_hexdump([], _Case, Acc) ->
    lists:flatten(lists:reverse(Acc));
list_to_hexdump([X | Xs], Case, Acc) ->
    Fmt = case Case of
        to_upper -> "~2.16.0B";
        to_lower -> "~2.16.0b"
    end,
    Hex = io_lib:format(Fmt, [X]),
    list_to_hexdump(Xs, Case, [Hex | Acc]).

%% ===================================================================
%% Tests begin
%% ===================================================================

-ifdef(TEST).

hexdump_to_list_test() ->
    ?assertEqual("hello", hexdump_to_list("68656C6C6F")),
    ?assertEqual("hello", hexdump_to_list("68656c6c6f")),
    ?assertEqual([1,15,255], hexdump_to_list("010FFF")),
    ?assertEqual([1,15,255], hexdump_to_list("010fff")).

hexdump_to_binary_test() ->
    ?assertEqual(<<"hello">>, hexdump_to_binary(<<"68656C6C6F">>)),
    ?assertEqual(<<"hello">>, hexdump_to_binary(<<"68656c6c6f">>)),
    ?assertEqual(<<1,15,255>>, hexdump_to_binary(<<"010FFF">>)),
    ?assertEqual(<<1,15,255>>, hexdump_to_binary(<<"010fff">>)).

list_to_hexdump_test() ->
    ?assertEqual("68656C6C6F", list_to_hexdump("hello", to_upper)),
    ?assertEqual("010FFF", list_to_hexdump([1,15,255], to_upper)),
    ?assertEqual("68656c6c6f", list_to_hexdump("hello", to_lower)),
    ?assertEqual("010fff", list_to_hexdump([1,15,255], to_lower)).

binary_to_hexdump_test() ->
    ?assertEqual(<<"68656C6C6F">>, binary_to_hexdump(<<"hello">>, to_upper)),
    ?assertEqual(<<"68656c6c6f">>, binary_to_hexdump(<<"hello">>, to_lower)).

-endif.

%% ===================================================================
%% Tests end
%% ===================================================================
