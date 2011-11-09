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
%%%     This is a utility module that complements an ets with 
%%%     lazy list aware functions.
%%% @end
%%% Created : Oct 22, 2011
%%%-------------------------------------------------------------------------------
-module(zlists_ets).

%%
%% Exported Functions
%%
-export([from_first/1,
         from_keyl/2,
         from_keyr/2,
         from_last/1,
         select/3,
         select_reverse/3,
         upload/3,
         upload/2]).

%%%%%%%%%%%%%%%%%%
%% API Functions
%%%%%%%%%%%%%%%%%%

%%-------------------------------------------------------------------------------
%% @doc
%%  Returns a lazy list iterating from first element to last.
%% @end
%%-------------------------------------------------------------------------------
-spec from_first(Tab :: ets:tab()) -> zlists:zlist(tuple()).
from_first(Tab) ->
    from_keyl(Tab,ets:first(Tab)).
%%-------------------------------------------------------------------------------
%% @doc
%%  Returns a lazy list iterating in normal order starting from specified key.
%% @end
%%-------------------------------------------------------------------------------
from_keyl(_Tab,'$end_of_table') ->
    [];
from_keyl(Tab,Key) ->
    case ets:lookup(Tab, Key) of
        [] -> [];
        Objects ->
            case ets:next(Tab, Key) of
                '$end_of_table' -> Objects;
                NextKey ->
                    zlists:new(Objects, fun()-> from_keyl(Tab,NextKey) end)
            end
    end.
%%-------------------------------------------------------------------------------
%% @doc
%%  Returns a lazy list iterating from last element to first.
%% @end
%%-------------------------------------------------------------------------------
-spec from_last(Tab :: ets:tab()) -> zlists:zlist(zlists:zlist(tuple())).
from_last(Tab) ->
    from_keyr(Tab,ets:last(Tab)).
%%-------------------------------------------------------------------------------
%% @doc
%%  Returns a lazy list iterating in reverse order starting from specified key.
%% @end
%%-------------------------------------------------------------------------------
from_keyr(_Tab,'$end_of_table') ->
    [];
from_keyr(Tab,Key) ->
    case ets:lookup(Tab, Key) of
        [] -> [];
        Objects ->
            case ets:prev(Tab, Key) of
                '$end_of_table' -> Objects;
                NextKey ->
                    zlists:new(Objects, fun()-> from_keyr(Tab,NextKey) end)
            end
    end.
%%-------------------------------------------------------------------------------
%% @doc
%%  This function is analogous to the one in an ets but returns a lazy list.
%% @end
%%-------------------------------------------------------------------------------
-spec select(Tab :: ets:tab(), 
             MatchSpec :: ets:match_spec(), 
             Limit :: non_neg_integer()) -> zlists:zlist(term()).
select(Tab, MatchSpec, Limit) ->
    select_(ets:select(Tab, MatchSpec, Limit)).

select_('$end_of_table') ->
    [];
select_({List,Cont}) ->
    zlists:new(List, fun()-> select_(ets:select(Cont)) end).
%%-------------------------------------------------------------------------------
%% @doc
%%  This function is analogous to the one in an ets but returns a lazy list.
%% @end
%%-------------------------------------------------------------------------------
-spec select_reverse(Tab :: ets:tab(), 
                     MatchSpec :: ets:match_spec(), 
                     Limit :: non_neg_integer()) -> zlists:zlist(term()).
select_reverse(Tab,MatchSpec, Limit) ->
    select_reverse_(ets:select_reverse(Tab, MatchSpec, Limit)).

select_reverse_('$end_of_table') ->
    [];
select_reverse_({List,Cont}) ->
    zlists:new(List, fun()-> select_(ets:select_reverse(Cont)) end).

%%-------------------------------------------------------------------------------
%% @doc
%%  Uploads an ets table with an objects from a lazy list using specified 
%%  batch size.
%% @end
%%-------------------------------------------------------------------------------
-spec upload(Tab :: ets:tab(), ZList :: zlists:zlist(tuple()), BatchSize :: non_neg_integer()) -> ok.
upload(Tab, ZList, BatchSize) ->
    case zlists:scroll(BatchSize, ZList) of
        {Page,[]} ->
            ets:insert(Tab, Page),
            ok;
        {Page,ZTail} ->
            ets:insert(Tab, Page),
            upload(Tab, ZTail, BatchSize)
    end.

%%-------------------------------------------------------------------------------
%% @doc
%%  The same as uploda/3 but batch size is equal to 1000.
%% @end
%%-------------------------------------------------------------------------------
-spec upload(Tab :: ets:tab(), ZList :: zlists:zlist(tuple())) -> ok.
upload(Tab, ZList) ->
    upload(Tab, ZList, 1000).
