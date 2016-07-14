%% Author: vvorobyov
%% Created: Nov 3, 2011
%% Description: TODO: Add description to zlists_randoms
-module(zlists_random).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([uniform_floats/0, uniform_floats/1]).

%%
%% API Functions
%%

uniform_floats() ->
    RandFlo=rand:uniform(),
    zlists:new([RandFlo], fun()-> uniform_floats() end).

uniform_floats(State0) ->
    {Fl, State1}=rand:uniform_s(State0),
    zlists:new([Fl],fun()-> uniform_floats(State1) end).

%%
%% Local Functions
%%
