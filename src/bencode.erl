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
%%% Bencoding FSM schema:
%%%
%%%  _____________________            ________________________
%%% |                     |  +------>|                        |
%%% | integer:            |  |       | string:                |
%%% | <<$i,Char,$e>>      |  |       | <<Char,$:,Bytes>>      |
%%% | where integer(Char) |  |       | where integer(Char)    |
%%% |_____________________|<------+  |       bitstring(Bytes) |
%%%    /_\                   |    |  |________________________|
%%%     |                    |    |                 /_\
%%%     |                    |    |                  |
%%%     |                    |    |                  |
%%%  ___|_________________   |    |   _______________|________
%%% |                     |--+    +--|                        |
%%% | list:               |          | dictionary:            |
%%% | <<$l,Content,$e>>   |--------->| <<$d,Content,$e>>      |
%%% |_____________________|<---------|________________________|
%%%
%%% ------------------------------------------------------------------
%%%
%%% @author Mathieu Kerjouan
%%% @copyright (c) 2017, Mathieu Kerjouan <contact [at] steepath.eu>
%%% @doc
%%% @end
%%%
%%%-------------------------------------------------------------------

-module(bencode).
-export([encode/1, encode/2, encode_options/0]).
-export([decode/1, decode/2, decode_options/0]).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec decode_options() -> list().
decode_options() ->
    [integer_as_string
    ,integer_as_bitstring
    ,string_as_list
    ,dictionary_as_proplists
    ,dictionary_as_map].

%%--------------------------------------------------------------------
%% @doc 
%% @end
%% @param bencoded string as bitstring or binary term
%% @returns erlang term based on bencode schema.
%%--------------------------------------------------------------------
-spec decode(bitstring() | list()) 
	    -> list() | map() | integer() | bitstring().
decode(Bitstring) -> 
    decode(Bitstring, []).

%%--------------------------------------------------------------------
%% @doc 
%% @end
%% @param bencoded string as bitstring or binary term
%% @param options list
%% @returns erlang term based on bencode schema. 
%%--------------------------------------------------------------------
-spec decode(bitstring() | list(), list()) 
	    -> list() | map() | integer() | bitstring().
decode(List, Opts)
  when is_list(List) ->
    decode(erlang:list_to_bitstring(List), Opts);
decode(Bitstring, _) ->
    bencode_decode:switch(Bitstring).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec encode_options() -> list().
encode_options() ->
    [integer_as_string
    ,integer_as_bitstring
    ,string_as_list
    ,dictionary_as_proplists
    ,dictionary_as_map].

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
encode_test() ->
    ?assertEqual(encode([1,2,3,[1,2,3]]),
		{ok, <<"li1ei2ei3eli1ei2ei3eee">>}),
    ?assertEqual(encode([#{a => 1}, 1, 23]),
		{ok, <<"ld1:ai1eei1ei23ee">>}).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec encode(integer() | bitstring() | list() | map())
	    -> {ok, bitstring()}.
encode(Data) 
  when is_integer(Data) ->
    bencode_encode:integer(Data);
encode(Data) 
  when is_bitstring(Data) ->
    bencode_encode:string(Data);
encode(Data) 
  when is_list(Data) ->
    bencode_encode:list(Data);
encode(Data) 
  when is_map(Data) ->
    bencode_encode:dictionary(Data).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
encode(_,_) ->
    ok.
