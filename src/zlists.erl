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
%%% 	This module supports basic operations over lazy/infinite lists 
%%%		represented as [X1,...,Xn|TailFun], where N >= 1, and 
%%%		TailFun is a function that takes no arguments and 
%%%		returns a proper Erlang list or again lazy list. 
%%%		
%%%		This is one of many other possible representations of a lazy 
%%%		lists but this one is a useful for those applications that 
%%%		faced by me.
%%%		
%%%		Also, one can ask "Why lazy lists? What for?" or 
%%%		"How to use them?". Its a matter of taste or style of 
%%%		programming. Those who are familiar with Lisp, Clojure, 
%%%		Haskel knows the answers. Those who from Java or C++ may 
%%%		remember iterators.
%%% @end
%%% Created : Oct 22, 2011
%%%-------------------------------------------------------------------
-module(zlists).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([new/2,
         generate/2,
         recurrent/2,
         recurrent/3,
         foreach/2, 
         foldl/3, 
         map/2, 
         seq/3, 
         dropwhile/2, 
         filter/2, 
         expand/1,
         expand/2,
         append/1,
         scroll/2,
         merge/2,
         merge/3,
         merge/1,
         print/1]).

-define(EXPAND(Tail), if is_function(Tail, 0) -> Tail(); true -> Tail end).

-type zlist(T) :: [T] | maybe_improper_list(T, fun( () -> zlist(T) ) ) .
-type zlist() :: zlist(any()) .

-export_type([zlist/1, zlist/0]).

%%%%%%%%%%%%%%%%%%
%% API Functions
%%%%%%%%%%%%%%%%%%

%%-------------------------------------------------------------------------------
%% @doc
%%  Creates a lazy list from a list (proper or lazy) and tail function. When 
%%	iterating through such a lazy list firstly elements from passed list go 
%%	and when it exhausted, tail function called which may return an another 
%%	lazy list or a proper list (e.g. empty).
%% @end
%%-------------------------------------------------------------------------------
-spec new(ZList :: zlist(T), Fun :: fun( () -> zlist(T) ) ) -> zlist(T) .

new([],Fun) when is_function(Fun, 0) ->
    Fun();
new([E],Fun) when is_function(Fun, 0) ->
    [E|Fun];
new([H|Tail],Fun) when is_function(Fun, 0) ->
    [H|fun() -> new(?EXPAND(Tail), Fun) end].

%%-------------------------------------------------------------------------------
%% @doc
%%  Creates a zlist based on a zlist of seeds and generation function. 
%%	Semantically it is equivalent to the following code:
%%		[ E || Seed <- SeedList, E <- GeneratorFun(Seed)],
%%	but the result of this function is lazy.
%%	
%%	This function can be easily composed with map and append, so it may be 
%%	considered as a sugar.
%% @end
%%-------------------------------------------------------------------------------
-spec generate(SeedList :: zlist(T), GeneratorFun :: fun( (T) -> zlist(T1) )) -> zlist(T1) .

generate([], _GeneratorFun) ->
    [];
generate([H|Tail], GeneratorFun) when is_function(GeneratorFun, 1) ->
    new(GeneratorFun(H),
        fun()-> generate(?EXPAND(Tail),GeneratorFun) end).

%%-------------------------------------------------------------------------------
%% @doc
%%  Creates an infinit zlist based on a recurrent formula. I.e. each next item 
%%	computed based on a previous item.
%% @end
%%-------------------------------------------------------------------------------
-spec recurrent(X0 :: T, RecFun :: fun( (T) -> T )) -> zlist(T) .

recurrent(X0, RecFun) ->
    new([X0], fun()-> recurrent(RecFun(X0), RecFun) end).

%%-------------------------------------------------------------------------------
%% @doc
%%  Creates an infinit zlist based on a recurrent formula with some inner state. 
%%	I.e. each next item and next state computed based on a previous item and a 
%%	previous state but only item visible as output in a zlist.
%%
%%	Try Fibonacci sequence:
%%		1>Fibs=zlists:reccurent(1, 0, fun(X0,S0) -> {X0+S0, X0} end).
%%		[1|#Fun<zlists.3.75807053>]
%%		2>zlists:expand(Fibs, 10).
%%		[1,1,2,3,5,8,13,21,34|#Fun<zlists.3.75807053>]
%% @end
%%-------------------------------------------------------------------------------
-spec recurrent(X0 :: T, S0 :: T1, RecFun :: fun( (T, T1) -> {T, T1} )) -> zlist(T) .

recurrent(X0, S0, RecFun) ->
    new([X0], fun()-> {X1,S1}=RecFun(X0,S0), recurrent(X1, S1, RecFun) end).

%%-------------------------------------------------------------------------------
%% @doc
%%  Just a lazy analog of lists:foreach. Note that the passed zlist may be 
%%	an infinite by nature, so be sure that you realy want to use foreach function 
%%	against such a zlist (in some cases it may have a sense if an infinit loop is
%%	wanted behaviour).
%% @end
%%-------------------------------------------------------------------------------
-spec foreach(Fun :: fun( (T) -> any() ), ZList :: zlist(T)) -> ok .

foreach(F, [Hd|Tail]) ->
    F(Hd),
    foreach(F, ?EXPAND(Tail));
foreach(F, []) when is_function(F, 1) -> ok.

%%-------------------------------------------------------------------------------
%% @doc
%%  Just a lazy analog of lists:foldl. Note that the passed zlist may be 
%%	an infinite by nature, so be sure that you realy want to use foldl function 
%%	against such a zlist (in some cases it may have a sense if an infinit loop
%%	with state is wanted behaviour).
%% @end
%%-------------------------------------------------------------------------------

-spec foldl(Fun, Acc0, ZList) -> Acc1 when
      Fun :: fun((Elem :: T, AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      ZList :: zlist(T).

foldl(F, Accu, [Hd|Tail]) ->
    foldl(F, F(Hd, Accu), ?EXPAND(Tail));
foldl(F, Accu, []) when is_function(F, 2) -> Accu.

%%-------------------------------------------------------------------------------
%% @doc
%%  Just a lazy analog of lists:map. Note that map function application is not 
%%	done at once for all elements in a zlist, application occurs as zlist 
%%	expanded/scrolled. 
%% @end
%%-------------------------------------------------------------------------------
-spec map(Fun, ZList1) -> ZList2 when
      Fun :: fun((A) -> B),
      ZList1 :: zlist(A),
      ZList2 :: zlist(B),
      A :: term(),
      B :: term().

map(F, [H|T]) ->
    [F(H)|fun()-> map(F, ?EXPAND(T)) end];
map(F, []) when is_function(F, 1) -> [].

%%-------------------------------------------------------------------------------
%% @doc
%%  Just a lazy analog of lists:seq/3.
%% @end
%%-------------------------------------------------------------------------------
-spec seq(From, To, Incr) -> Seq when
      From :: integer(),
      To :: integer() | infinity,
      Incr :: integer(),
      Seq :: [integer()].

seq(_First, _Last, 0) ->
    [];
seq(First, Last, Inc) when Inc>0, First>Last, is_integer(Last) ->
    [];
seq(First, Last, Inc) when Inc<0, First<Last, is_integer(Last) ->
    [];
seq(First, Last, Inc) -> 
    [First|fun()-> seq(First+Inc, Last, Inc) end].

%%-------------------------------------------------------------------------------
%% @doc
%%  Just a lazy analog of lists:dropwhile/2. It skips the first elements 
%%	satisfying predicate and returns a tail either list or zlist.
%% @end
%%-------------------------------------------------------------------------------
-spec dropwhile(Pred, ZList1) -> ZList2 when
      Pred :: fun((Elem :: T) -> boolean()),
      ZList1 :: zlist(T),
      ZList2 :: zlist(T).

dropwhile(Pred, [Hd|Tail]=Rest) ->
    case Pred(Hd) of
        true -> dropwhile(Pred, ?EXPAND(Tail));
        false -> Rest
    end;
dropwhile(Pred, []) when is_function(Pred, 1) -> [].

%%-------------------------------------------------------------------------------
%% @doc
%%  A lazy analog of lists:filter/2. It filters zlist lazely as list 
%%	expanded/scrolled.
%% @end
%%-------------------------------------------------------------------------------
-spec filter(Pred, ZList1) -> ZList2 when
      Pred :: fun((Elem :: T) -> boolean()),
      ZList1 :: zlist(T),
      ZList2 :: zlist(T).

filter(Pred, ZList) when is_function(Pred, 1) ->
    Pred1=fun(E)-> not Pred(E) end,
    case dropwhile(Pred1, ZList) of
        [] -> [];
        [_]=R -> R;
        [H|T] -> [H| fun()-> filter(Pred, ?EXPAND(T)) end]
    end.

%%-------------------------------------------------------------------------------
%% @doc
%%  Unlazies a zlist. I.e. creates a proper list from zlist.
%% @end
%%-------------------------------------------------------------------------------
expand([]) ->
    [];
expand([H|T]) ->
    [H|expand(?EXPAND(T))].

%%-------------------------------------------------------------------------------
%% @doc
%%  Partially unlazies a zlist. Using a specified head lengh N creates a new zlist 
%%  with first N elements available for pattern matching.
%% @end
%%-------------------------------------------------------------------------------
expand([],_N) ->
    [];
expand([_,_,_,_|_]=List,4) ->
    List;
expand([_,_,_|_]=List,3) ->
    List;
expand([_,_|_]=List,2) ->
    List;
expand([_|_]=List,1) ->
    List;
expand([H|T],N) ->
    [H|expand(?EXPAND(T),N-1)].

%%-------------------------------------------------------------------------------
%% @doc
%%   A lazy analog of lists:append/1.
%% @end
%%-------------------------------------------------------------------------------
-spec append(ZListOfZLists:: zlist(zlist(T))) -> zlist(T) when T :: term().

append([]) ->
    [];
append([ZList]) ->
    ZList;
append([ZList | OtherZLists]) ->
    new(ZList, fun()-> append(?EXPAND(OtherZLists)) end).

%%-------------------------------------------------------------------------------
%% @doc
%%  This function helps to iterate through zlist. It cuts oh a head of a lenght 
%%  N and returns this head and a lazy tail.
%% @end
%%-------------------------------------------------------------------------------

-spec scroll(N, ZList1) -> {List2, ZList3} when
      N :: non_neg_integer(),
      ZList1 :: zlist(T),
      List2 :: [T],
      ZList3 :: zlist(T),
      T :: term().

scroll(N, ZList) when is_integer(N), N >= 0, is_list(ZList) ->
    Exp=zlists:expand(ZList, N),
    try lists:split(N, Exp) of
        {Page,Tail} -> {Page,?EXPAND(Tail)}
    catch
        error:badarg ->
            {Exp, []}
    end.

%%-------------------------------------------------------------------------------
%% @doc
%%   A lazy analog of lists:merge/2.
%% @end
%%-------------------------------------------------------------------------------

-spec merge(ZList1 :: zlist(), ZList2 :: zlist()) -> zlist().

merge([], ZList2) ->
    ZList2;
merge(ZList1, []) ->
    ZList1;
merge([H1|Tail1], [H2|_]=ZList2) when H1 =< H2 ->
    [H1| fun()-> merge(?EXPAND(Tail1),ZList2) end];
merge(ZList1, [H2|Tail2]) ->
    [H2| fun()-> merge(ZList1,?EXPAND(Tail2)) end].

%%-------------------------------------------------------------------------------
%% @doc
%%   A lazy analog of lists:merge/3.
%% @end
%%-------------------------------------------------------------------------------

-spec merge(Fun, List1, List2) -> List3 when
      Fun :: fun((A, B) -> boolean()),
      List1 :: zlist(A),
      List2 :: zlist(B),
      List3 :: zlist((A | B)).

merge(_Fun, [], ZList2) ->
    ZList2;
merge(_Fun, ZList1, []) ->
    ZList1;
merge(Fun, [H1|Tail1]=ZList1, [H2|Tail2]=ZList2) ->
    H1_not_greater_than_H2=Fun(H1,H2),
    if H1_not_greater_than_H2 ->
           [H1| fun()-> merge(Fun,?EXPAND(Tail1),ZList2) end];
       true -> 
           [H2| fun()-> merge(Fun,ZList1,?EXPAND(Tail2)) end]
    end.

%%-------------------------------------------------------------------------------
%% @doc
%%   A lazy analog of lists:merge/1.
%% @end
%%-------------------------------------------------------------------------------

-spec merge(ListOfZLists :: [zlist()]) -> zlist().

merge([]) ->
    [];
merge([ZList1]) ->
    ZList1;
merge([ZList1,ZList2|Other]) ->
    merge([merge(ZList1, ZList2)|Other]).

%%-------------------------------------------------------------------------------
%% @doc
%%  Debug function to print zlists. Do not use it for infinit zlists.
%% @end
%%-------------------------------------------------------------------------------
print(L) ->
    foreach(fun(E)-> io:format("~p~n",[E]) end, L).

%%
%% Local Functions
%%

%%
%% eUnit Functions
%%
append_list_of_lists_test() ->
    L1=[1,2,3],
    L2=[4,5,6],
    Lr=L1++L2,
    Lr=expand(append([L1, L2])).
append_list_of_zlists_test() ->
    L1=[1,2,3],
    L2=[4,5,6],
    Lr=L1++[a]++L2++[7,8],
    ZL1=new(L1,fun()->[a]end),
    ZL2=new(L2,fun()->[7,8]end),
    Lr=expand(append([ZL1, ZL2])).
append_zlist_of_zlists_test() ->
    L1=[1,2,3],
    L2=[4,5,6],
    ZL1=new(L1,fun()->[a]end),
    ZL2=new(L2,fun()->[7,8]end),
    ZL3=new([0],fun()->[c,d,e]end),
    Lr=L1++[a]++L2++[7,8]++[0,c,d,e],
    Lr=expand(append(new([ZL1],fun()-> [ZL2,ZL3] end))).
