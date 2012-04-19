%%%-------------------------------------------------------------------
%%% @author <vjache@gmail.com>
%%% @copyright (C) 2011, Vyacheslav Vorobyov.  All Rights Reserved.
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @doc
%%%     This is a utility module that complements a file module with 
%%%     lazy list aware functions.
%%% @end
%%% Created : Apr 19, 2011
%%%-------------------------------------------------------------------------------
-module(zlists_file).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([read/2, expand_binary/2]).

%%
%% API Functions
%%

%%-------------------------------------------------------------------------------
%% @doc
%%  Returns a content of a file as a Z-List.
%% @end
%%-------------------------------------------------------------------------------
-spec read(IoDevice :: file:io_device(), 
           PageSize :: non_neg_integer()) -> zlists:zlist( binary() | list( non_neg_integer() ) ).

read(Fd, Sz) ->
    case file:read(Fd, Sz) of
        {ok, Data} when is_binary(Data) ->
            [Data | fun()-> read(Fd, Sz) end];
        {ok, Data} when is_list(Data) ->
            zlists:new(Data,fun()-> read(Fd, Sz) end);
        eof -> [];
        {error, Reason} -> throw(Reason)
    end.

%%-------------------------------------------------------------------------------
%% @doc
%%  Tries to make a binary head of Z-List to have at least BinSize bytes. This 
%%  function is aplicable only against Z-List of binaries. 
%% @end
%%-------------------------------------------------------------------------------
-spec expand_binary(BinZList :: zlists:zlist(binary()), 
                    BinSize  :: non_neg_integer()) -> zlists:zlist(binary()).

expand_binary([Bin|ZTail],Sz) when byte_size(Bin) >= Sz ->
    <<Chunk:Sz/binary,BTail/binary>> = Bin,
    [Chunk,BTail|ZTail];
expand_binary([Bin],Sz) when byte_size(Bin) < Sz ->
    [Bin];
expand_binary([Bin|ZTail],Sz) ->
    case zlists:expand(1,ZTail) of
        [H|ZTail1]  -> expand_binary([<<Bin/binary,H/binary>>|ZTail1],Sz);
        []          -> [Bin]
    end.

%%
%% Local Functions
%%

