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
%%%     This is a utility module that complements a file with 
%%%     lazy list aware functions.
%%% @end
%%% Created : Oct 22, 2011
%%%-------------------------------------------------------------------------------
-module(zlists_file).

%%
%% Exported Functions
%%
-export([scan_dir/2, scan_dirs/2]).

%%%%%%%%%%%%%%%%%%
%% API Functions
%%%%%%%%%%%%%%%%%%

%%-------------------------------------------------------------------------------
%% @doc
%%  Returns a lazy list of all(recursively) files & directories satisfying 
%%  to wildcard.
%% @end
%%-------------------------------------------------------------------------------
scan_dir(Wildcard,Cwd)->
    scan_dirs(Wildcard,[Cwd]).

scan_dirs(Wildcard,Dirs)->
    zlists:generate(
      Dirs, 
      fun(D)->
              All=[filename:join(D, F) || F <- filelib:wildcard(Wildcard, D)],
              zlists:new(All, 
                         fun()->
                                 {ok,Filenames}=file:list_dir(D),
                                 FullNames=[filename:join(D, F) || F <- Filenames],
                                 scan_dirs(Wildcard,lists:filter(fun filelib:is_dir/1, FullNames)) 
                         end)
      end).

