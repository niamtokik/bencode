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

-module(bencode_decode).
-export([integer/1]).
-export([string/1]).
-export([list/1]).
-export([dictionary/1]).
-export([switch/1]).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc switch/1 helper function will route bitstring parameter
%%      based on pattern matching. 
%% @end
%% @param bencoded string as bitstring.
%% @return erlang term
%%--------------------------------------------------------------------
switch(Bitstring) ->
    case Bitstring of
	<<"i", _/bitstring>> ->
	    switch_integer(Bitstring);
	<<"l", _/bitstring>> ->
	    switch_list(Bitstring);
	<<"d", _/bitstring>> ->
	    switch_dictionary(Bitstring);
	<<Char, _/bitstring>> 
	  when Char >= $0 andalso Char =< $9 ->
	    switch_string(Bitstring);
	_Else ->
	    {error, not_bencoded}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%% @param 
%%--------------------------------------------------------------------
switch_integer(Bitstring) -> 
    integer(Bitstring).

%%--------------------------------------------------------------------
%% @doc
%% @end
%% @param
%%--------------------------------------------------------------------
switch_list(Bitstring) -> 
    list(Bitstring).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
switch_dictionary(Bitstring) -> 
    dictionary(Bitstring).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
switch_string(Bitstring) -> 
    string(Bitstring).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
integer_test() ->
    ?assertEqual(integer(<<"test">>), {not_integer, <<"test">>}),
    ?assertEqual(integer(<<"ie">>), {error, invalid}),
    ?assertEqual(integer(<<"i0e">>), {ok, 0}),
    ?assertEqual(integer(<<"i0etest">>), {ok, 0, <<"test">>}),
    ?assertEqual(integer(<<"i-0e">>), {error, invalid}),
    ?assertEqual(integer(<<"i-00001e">>), {error, invalid}),
    ?assertEqual(integer(<<"i000001e">>), {error, invalid}),
    ?assertEqual(integer(<<"i1e">>), {ok, 1}),
    ?assertEqual(integer(<<"i3e">>), {ok, 3}),
    ?assertEqual(integer(<<"i-3e">>), {ok, -3}),
    ?assertEqual(integer(<<"i03e">>), {error, invalid}),
    ?assertEqual(integer(<<"i1etest">>), {ok, 1, <<"test">>}),
    % 64bit value
    ?assertEqual(integer(<<"i18446744073709551616e">>), 
		 {ok, 18446744073709551616}),
    % 64bit negative value
    ?assertEqual(integer(<<"i-18446744073709551616e">>), 
		 {ok, (-18446744073709551616)}),
    % 128bit value
    ?assertEqual(integer(<<"i340282366920938463463374607431768211456e">>), 
		 {ok, 340282366920938463463374607431768211456}),
    % 128bit negative value
    ?assertEqual(integer(<<"i-340282366920938463463374607431768211456e">>),
		 {ok, (-340282366920938463463374607431768211456)}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%% @param bencoded string as bitstring or binary.
%% @returns tuple with atom() tag and erlang integer term.
%%--------------------------------------------------------------------
-spec integer(bitstring() | list()) 
		    -> {ok, integer()} | 
		       {ok, integer(), bitstring()} |
		       {error, term()}.
integer(List) when is_list(List) ->
    integer(erlang:list_to_bitstring(List));
integer(<<"ie">>) ->
    {error, invalid};
integer(<<"i0e">>) ->
    {ok, 0};
integer(<<"i0e", Rest/bitstring>>) ->
    {ok, 0, Rest};
integer(<<$i, Rest/bitstring>>) ->
    integer(Rest, <<>>);
integer(_Else) 
  when is_bitstring(_Else) ->
    {not_integer, _Else}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%% @param
%% @returns
%%--------------------------------------------------------------------
-spec integer(bitstring(), bitstring()) 
		    -> {ok, integer()} |
		       {ok, integer(), bitstring()} |
		       {error, term()}.
integer(<<$0, _/bitstring>>, <<>>) ->
    {error, invalid};
integer(<<$-, $0, _/bitstring>>, <<>>) ->
    {error, invalid};
integer(<<Number, Rest/bitstring>>, <<>>) 
  when (Number >= $1 andalso Number =< $9) orelse Number =:= $- ->
    integer(Rest, <<Number>>);
integer(<<Number, Rest/bitstring>>, Buf) 
  when Number >= $0 andalso Number =< $9 ->
    integer(Rest, <<Buf/bitstring, Number>>);
integer(<<$e>>, Buf) ->
    {ok, erlang:binary_to_integer(Buf)};
integer(<<$e, Rest/bitstring>>, Buf) ->
    {ok, erlang:binary_to_integer(Buf), Rest};
integer(_, _) ->
    {error, invalid}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
string_test() ->
    ?assertEqual(string(<<"0:">>), {ok, <<"">>}),
    ?assertEqual(string(<<"1:a">>), {ok, <<"a">>}),
    ?assertEqual(string(<<"2:be">>), {ok, <<"be">>}),
    ?assertEqual(string(<<"4:test">>), {ok, <<"test">>}),
    ?assertEqual(string(<<"5:">>), {error, bad_length}),
    ?assertEqual(string(<<"6:jijoja4:test">>), 
		 {ok, <<"jijoja">>, <<"4:test">>}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec string(bitstring() | list()) 
		   -> {ok, bitstring()} |
		      {ok, bitstring(), bitstring()} |
		      {error, bad_length} |
		      {not_string, bitstring()}.

string(List)
  when is_list(List) ->
    string(erlang:list_to_bitstring(List));
string(<<"0:">>) ->
    {ok, <<"">>};
string(<<Number, Rest/bitstring>>) 
  when Number >= $1 andalso Number =< $9 ->
    string(Rest, <<Number>>);
string(_Else) ->
    {not_string, _Else}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec string(bitstring(), bitstring())
		   -> {ok, bitstring()} |
		      {ok, bitstring(), bitstring()} |
		      {error, bad_length}.
			   
string(<<$:, Rest/bitstring>>, Length) ->
    string(Rest, erlang:binary_to_integer(Length), <<>>);
string(<<Number, Rest/bitstring>>, Length) 
  when Number >= $0 andalso Number =< $9 ->
    string(Rest, <<Length/bitstring, Number>>).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec string(bitstring(), non_neg_integer(), bitstring())
		   ->{ok, bitstring()} |
		      {ok, bitstring(), bitstring()} |
		      {error, bad_length}.
			   
string(<<>>, 0, String) ->
    {ok, String};
string(<<>>, _, _) ->
    {error, bad_length};
string(Rest, 0, String) ->
    {ok, String, Rest};
string(<<Char, Rest/bitstring>>, Length, String) ->
    string(Rest, Length-1, <<String/bitstring, Char>>).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
list_test() ->
    ?assertEqual(list(<<"l4:spam4:eggse">>),
		 {ok, [<<"spam">>, <<"eggs">>]}),
    ?assertEqual(list(<<"le">>), 
		 {ok, []}),
    ?assertEqual(list(<<"ldee">>), {ok, [#{}]}),
    ?assertEqual(list(<<"llleee">>), {ok, [[[]]]}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec list(bitstring()) 
		 -> {ok, list()} |
		    {ok, list(), bitstring()} |
		    {error, no_ending_delimiter} |
		    {not_list, bitstring()}.
			 
list(<<$l, Rest/bitstring>>) ->
    list(Rest, []);
list(_Else) ->
    {not_list, _Else}.

-spec list(bitstring(), list()) 
		 -> {ok, list()} |
		    {ok, list(), bitstring()} |
		    {error, term()}.
list(<<$e>>, List) ->
    {ok, lists:reverse(List)};
list(<<$e, Rest/bitstring>>, List) ->
    {ok, lists:reverse(List), Rest};
list(Bitstring, Buf) ->
    case switch(Bitstring) of
	{ok, _} ->
	    {error, no_ending_delimiter};
	{ok, Data, Rest} ->
	    list(Rest, [Data] ++ Buf)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
dictionary_test() ->
    ?assertEqual(dictionary(<<"de">>), {ok, #{}}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec dictionary(bitstring()) 
		       -> {ok, map()} |
			  {ok, map(), bitstring()} |
			  {error, no_value} |
			  {error, no_ending_delimiter} |
			  {not_dictionary, bitstring()}.
			       
dictionary(<<"de">>) ->
    {ok, #{}};
dictionary(<<"de", Rest/bitstring>>) ->
    {ok, #{}, Rest};
dictionary(<<$d, Rest/bitstring>>) ->
    dictionary(Rest, #{}, {});
dictionary(_Else) ->
    {not_dictionary, _Else}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec dictionary(bitstring(), map()) 
		       -> {ok, map()} |
			  {ok, map(), bitstring()} |
			  {error, term()}.			       
dictionary(<<$e>>, Dict) ->
    {ok, Dict};
dictionary(<<$e, Rest/bitstring>>, Dict) ->
    {ok, Dict, Rest};
dictionary(Bitstring, Dict) ->
    dictionary(Bitstring, Dict, {}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec dictionary(bitstring(), map(), tuple()) 
		       -> {ok, map()} |
			  {ok, map(), bitstring()} |
			  {error, term()}.
dictionary(<<>>, _, {_}) ->
    {error, no_value};
dictionary(<<>>, _, {_, _}) ->
    {error, no_ending_delimiter};
dictionary(<<$e, _/bitstring>>, _, {_}) ->
    {error, no_value};
dictionary(Bitstring, Dict, {}) ->
    case switch(Bitstring) of
	{ok, _} ->
	    {error, no_ending_delimiter};
	{ok, Key, Rest} ->
	    dictionary(Rest, Dict, {Key})
    end;
dictionary(Bitstring, Dict, {Key}) ->
    case switch(Bitstring) of
	{ok, _} ->
	    {error, no_ending_delimiter};
	{ok, Value, Rest} ->
	    dictionary(Rest, Dict, {Key, Value})
    end;
dictionary(Bitstring, Dict, {Key, Value}) ->
    dictionary(Bitstring, maps:put(Key, Value, Dict)).
