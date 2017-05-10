%%%-------------------------------------------------------------------
%%% Copyright (c) 2017, Mathieu Kerjouan <contact [at] steepath.eu>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code  must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions  in  binary  form  must  reproduce  the   above
%%%    copyright  notice, this  list of  conditions and  the following
%%%    disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. All  advertising materials mentioning  features or use  of this
%%%    software  must  display  the  following  acknowledgement:  This
%%%    product includes software developed by the <organization>.
%%%
%%% 4. Neither  the name of  the <organization>  nor the names  of its
%%%    contributors may be used to endorse or promote products derived
%%%    from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY  <COPYRIGHT HOLDER> ''AS IS'' AND ANY
%%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
%%% PURPOSE ARE  DISCLAIMED. IN NO  EVENT SHALL <COPYRIGHT  HOLDER> BE
%%% LIABLE FOR  ANY DIRECT, INDIRECT, INCIDENTAL,  SPECIAL, EXEMPLARY,
%%% OR  CONSEQUENTIAL   DAMAGES  (INCLUDING,   BUT  NOT   LIMITED  TO,
%%% PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%% PROFITS;  OR  BUSINESS INTERRUPTION)  HOWEVER  CAUSED  AND ON  ANY
%%% THEORY  OF LIABILITY,  WHETHER IN  CONTRACT, STRICT  LIABILITY, OR
%%% TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
%%% THE USE  OF THIS SOFTWARE, EVEN  IF ADVISED OF THE  POSSIBILITY OF
%%% SUCH DAMAGE.
%%%
%%% ------------------------------------------------------------------
%%%
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2017, Mathieu Kerjouan <contact [at] steepath.eu>
%%% @doc
%%% @end
%%%
%%%-------------------------------------------------------------------

-module(bencode_encode).
-export([integer/1]).
-export([string/1]).
-export([list/1]).
-export([dictionary/1]).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
integer_test() ->
    ?assertEqual(integer(1), {ok, <<"i1e">>}),
    ?assertEqual(integer(-1), {ok, <<"i-1e">>}),
    ?assertEqual(integer(0), {ok, <<"i0e">>}),
    ?assertEqual(integer(1000), {ok, <<"i1000e">>}),
    ?assertEqual(integer(-1000), {ok, <<"i-1000e">>}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec integer(integer()) -> {ok, bitstring()}.
integer(Integer) 
  when is_integer(Integer) ->
    Bitstring = erlang:integer_to_binary(Integer),
    {ok, <<$i, Bitstring/bitstring, $e>>}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
string_test() ->
    ?assertEqual(string(<<"test">>), {ok, <<"4:test">>}),
    ?assertEqual(string(<<"">>), {ok, <<"0:">>}),
    ?assertEqual(string(a), {ok, <<"1:a">>}),
    ?assertEqual(string(test), {ok, <<"4:test">>}),
    ?assertEqual(string(<<"dumped">>), {ok, <<"6:dumped">>}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec string(bitstring()) -> {ok, bitstring()}.
string(List) 
  when is_list(List) ->
    string(erlang:list_to_bitstring(List));
string(Atom) when is_atom(Atom) ->
    string(erlang:atom_to_binary(Atom, utf8));
string(<<>>) ->
    {ok, <<"0:">>};
string(Bitstring) ->
    Length = erlang:byte_size(Bitstring),
    LengthBitstring = erlang:integer_to_binary(Length),
    {ok, <<LengthBitstring/bitstring,$:,Bitstring/bitstring>>}.
    

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
list_test() ->
    ?assertEqual(list([1,2,3]), 
		 {ok, <<"li1ei2ei3ee">>}),
    ?assertEqual(list([<<"a">>,<<"b">>,<<"c">>]), 
		 {ok, <<"l1:a1:b1:ce">>}),
    ?assertEqual(list([a,b,c]), 
		 {ok, <<"l1:a1:b1:ce">>}),
    ?assertEqual(list([#{}]), 
		 {ok, <<"ldee">>}),
    ?assertEqual(list([[]]),
		 {ok, <<"llee">>}),
    ?assertEqual(list([1,2,3,[a,b,c]]),
		{ok,<<"li1ei2ei3el1:a1:b1:cee">>}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec list(list()) -> {ok, bitstring()}.
list([]) ->
    {ok, <<"le">>};
list(List) 
  when is_list(List) ->
    list(List, <<>>).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec list(list(), bitstring()) -> {ok, bitstring()}.
list([], Buf) ->
    {ok, <<$l, Buf/bitstring, $e>>};
list([H|T], Buf) 
  when is_integer(H)->
    {ok, Integer} = integer(H),
    list(T, <<Buf/bitstring, Integer/bitstring>>);
list([H|T], Buf) 
  when is_atom(H) ->
    {ok, String} = string(H),
    list(T, <<Buf/bitstring, String/bitstring>>);
list([H|T], Buf) 
  when is_list(H) ->
    {ok, String} = list(H),
    list(T, <<Buf/bitstring, String/bitstring>>);
list([H|T], Buf) 
  when is_bitstring(H) ->
    {ok, String} = string(H),
    list(T, <<Buf/bitstring, String/bitstring>>);
list([{Key, Value}|T], Buf) 
  when is_bitstring(Key) ->
    {wip, proplist};
list([H|T], Buf) 
    when is_map(H) ->
    {ok, Dictionary} = dictionary(H),
    list(T, <<Buf/bitstring, Dictionary/bitstring>>).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
dictionary_test() ->
    ?assertEqual(dictionary(#{<<"test">> => []}), 
		 {ok, <<"d4:testlee">>}),
    ?assertEqual(dictionary(#{a => #{ b => c }}),
		 {ok,<<"d1:ad1:b1:cee">>}),
    ?assertEqual(dictionary(#{aaa => 3, bb => 2, c => 1 }),
		 {ok,<<"d1:ci1e2:bbi2e3:aaai3ee">>}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec dictionary(map()) -> {ok, bitstring()}.
dictionary(Map) 
  when is_map(Map), map_size(Map) =:= 0 ->
    {ok, <<"de">>};
dictionary(Map) 
  when is_map(Map), map_size(Map) >= 1 ->
    Keys = sort_map_keys(Map),
    Enc = fun(K, M) ->  
		  V = maps:get(K, M),
		  key_value(K, V)
	  end,
    Dict = << (Enc(Key, Map)) || Key <- Keys >>,
    {ok, <<$d, Dict/bitstring, $e>>}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
sort_map_keys_test() ->
    ?assertEqual(sort_map_keys(#{aaa => 3, bb => 2, c => 1}),
		 [c, bb, aaa]),
    ?assertEqual(sort_map_keys(#{<<"aaa">> => 3, <<"bb">> => 2, <<"c">> => 1 }),
		 [<<"c">>,<<"bb">>,<<"aaa">>]).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec sort_map_keys(map()) -> list().
sort_map_keys(Map) 
  when is_map(Map) ->
    Keys = maps:keys(Map),
    Fun = fun F(X) when is_atom(X) -> 
		  erlang:atom_to_binary(X, utf8);
	      F(X) when is_list(X) -> 
		  erlang:list_to_bitstring(X);
	      F(X) when is_integer(X) -> 
		  erlang:integer_to_binary(X);
	      F(X) -> X
	  end,
    Zip = [ {erlang:byte_size(Fun(X)), X} || X <- Keys ],
    SortedZip = lists:sort(Zip),
    {_, Sort} = lists:unzip(SortedZip),
    Sort.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
key_test() ->
    ?assertEqual(key(a), <<"1:a">>),
    ?assertEqual(key(1), <<"1:1">>),
    ?assertEqual(key(<<"a">>), <<"1:a">>).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec key(integer() | atom() | list() | bitstring())
		-> bitstring().
key(Key) when is_integer(Key) ->
    key(erlang:integer_to_binary(Key));
key(Key) when is_list(Key) ->
    key(erlang:list_to_bitstring(Key));
key(Key) when is_atom(Key) ->
    key(erlang:atom_to_binary(Key, utf8));
key(Key) when is_bitstring(Key) ->
    {ok, KeyBitstring} = string(Key),
    KeyBitstring.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
value_test() ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec value(integer() | atom() | list() | bitstring())
		  -> bitstring().
value(Value) when is_integer(Value) -> 
    {ok, ValueBitstring} = integer(Value),
    ValueBitstring;
value(Value) when is_bitstring(Value) -> 
    {ok, ValueBitstring} = string(Value),
    ValueBitstring;
value(Value) when is_atom(Value) -> 
    {ok, ValueBitstring} = string(Value),
    ValueBitstring;
value(Value) when is_list(Value) -> 
    {ok, ValueBitstring} = list(Value),
    ValueBitstring;
value(Value) when is_map(Value) -> 
    {ok, ValueBitstring} = dictionary(Value),
    ValueBitstring.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec key_value(bitstring(), integer() | list() | bitstring() | map() )
		      -> bitstring().
key_value(Key, Value) ->
    K = key(Key),
    V = value(Value),
    <<K/bitstring, V/bitstring>>.
