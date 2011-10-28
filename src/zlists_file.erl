%%%-------------------------------------------------------------------------------
%%% @author <vjache@gmail.com>
%%% @copyright (C) 2011, Vyacheslav Vorobyov
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

