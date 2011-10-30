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
%%%     This is a utility module that complements a disk_log with 
%%%     lazy list aware functions.
%%% @end
%%% Created : Oct 22, 2011
%%%-------------------------------------------------------------------------------
-module(zlists_disk_log).

%%
%% Exported Functions
%%
-export([read/2, read/1, write/2, write/3, awrite/2, awrite/3]).

%%%%%%%%%%%%%%%%%%
%% API Functions
%%%%%%%%%%%%%%%%%%

%%-------------------------------------------------------------------------------
%% @doc
%%  Returns a lazy list of an all terms in an accessible log.
%% @end
%%-------------------------------------------------------------------------------
read(Log) ->
    read(Log, 1000).
read(Log, BatchSize) ->
    read(Log, BatchSize, start).
read(Log, BatchSize, Cont) ->
    case disk_log:chunk(Log, Cont, BatchSize) of
        eof -> [];
        {error, Reason} -> throw(Reason);
        {Cont2, Terms} -> 
            zlists:new(Terms, fun()-> read(Log, BatchSize, Cont2) end);
        {Cont2, Terms, _Badbytes} -> 
            zlists:new(Terms, fun()-> read(Log, BatchSize, Cont2) end)
    end.

%%-------------------------------------------------------------------------------
%% @doc
%%  Writes synchronously a terms from a lazy list to an accessible log.
%% @end
%%-------------------------------------------------------------------------------
write(Log, ZList) ->
    write(Log, ZList, 1000).
write(Log, ZList, BatchSize) ->
    write(Log, ZList, BatchSize, fun disk_log:log_terms/2 ).
%%-------------------------------------------------------------------------------
%% @doc
%%  Writes asynchronously a terms from a lazy list to an accessible log.
%% @end
%%-------------------------------------------------------------------------------
awrite(Log, ZList) ->
    awrite(Log, ZList, 1000).
awrite(Log, ZList, BatchSize) ->
    write(Log, ZList, BatchSize, fun disk_log:alog_terms/2 ).

write(Log, ZList, BatchSize, LogFun) ->
    case zlists:scroll(BatchSize, ZList) of
        {Batch,[]} -> 
            ok=LogFun(Log, Batch);
        {Batch,ZList1} -> 
            ok=LogFun(Log, Batch),
            write(Log, ZList1, BatchSize, LogFun)
    end.