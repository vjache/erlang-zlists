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
         entail/2,
         generate/2,
         recurrent/2,
         recurrent/3,
         foreach/2, 
         foldl/3, 
         map/2, 
         seq/3,
         splitwith/2,
         dropwhile/2,
         drop/2,
         take/2,
         take2/2,
         takewhile/2,
         takewhile2/2,
         filter/2, 
         expand/1,
         expand/2,
         append/1,
         scroll/2,
         merge/2,
         merge/3,
         merge/1,
         merge_using/2,
         keymerge/2,
         keymerge/3,
         cartesian/2,
         join/1,
         join/3,
         zip/2,
         ziph/2,
         unique/1,
         unique/2,
         count/1,
         aggregate/4,
         print/1,
         print/3]).

-define(EXPAND(Tail), if is_function(Tail, 0) -> Tail(); true -> Tail end).

-type zlist(T) :: maybe_improper_list(T, fun(()-> zlist(T)) | [] ) .

-export_type([zlist/1]).

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
%%  Similar to the zlists:new/2 but used fun of arity 1, where an argument is a 
%%  last item of zlist passed. This function gives a chance to continue after 
%%  exhaustion of a first zlist passed with a one constructed based on last element.
%%  
%%  Example:
%%      1> L=zlists:entail([1,2,3,4,5], fun(N)-> lists:seq(N+1,N+10) end).
%%      [1|#Fun<zlists.30.2973546>]
%%      2> zlists:expand(L).
%%      [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
%% @end
%%-------------------------------------------------------------------------------
-spec entail(ZList :: zlist(T), Fun :: fun( (T) -> zlist(T) ) ) -> zlist(T) .
entail([],_Fun) ->
    [];
entail([E],Fun) ->
    [E|Fun(E)];
entail([E|TFun],Fun) when is_function(TFun, 0) ->
    [E|fun()-> case TFun() of [] -> Fun(E); ZL -> entail(ZL,Fun) end end];
entail([E|Tail],Fun)->
    [E|fun()-> entail(Tail,Fun) end].

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
%%		1>Fibs=zlists:recurrent(1, 0, fun(X0,S0) -> {X0+S0, X0} end).
%%		[1|#Fun<zlists.3.75807053>]
%%		2>zlists:expand(10, Fibs).
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
%%  Drops a first N elements of a specified zlist and returns its remainin tail (zlist).
%% @end
%%-------------------------------------------------------------------------------

-spec drop( N :: non_neg_integer(), ZList :: zlist(T)) -> zlist(T) .

drop( 0, Tail) ->
    Tail;
drop( N, [_|Tail]) ->
    drop(N-1,?EXPAND(Tail)).

%%-------------------------------------------------------------------------------
%% @doc
%%  Just a lazy analog of lists:takewhile/2. It returns first elements while 
%%  predicate function return true.
%% @end
%%-------------------------------------------------------------------------------
-spec takewhile(Pred, ZList1) -> ZList2 when
      Pred :: fun((Elem :: T) -> boolean()),
      ZList1 :: zlist(T),
      ZList2 :: zlist(T).

takewhile(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
        true -> [Hd | fun()-> takewhile(Pred, ?EXPAND(Tail)) end];
        false -> []
    end;
takewhile(Pred, []) when is_function(Pred, 1) -> [].

%%-------------------------------------------------------------------------------
%% @doc
%%  Same as takewhile/2 but "less lazy". If some first values already expanded 
%%  then 'takewhile' logic applied immediately to them until it faces tail 
%%  function. This function may exhibit higher performance due to less calls 
%%  to tail fun.
%% @end
%%-------------------------------------------------------------------------------
-spec takewhile2(Pred, ZList1) -> ZList2 when
      Pred :: fun((Elem :: T) -> boolean()),
      ZList1 :: zlist(T),
      ZList2 :: zlist(T).
takewhile2(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
        true when is_list(Tail) -> [Hd | takewhile2(Pred, Tail)];
        true when is_function(Tail,0)-> [Hd | fun()-> takewhile2(Pred, Tail()) end];
        false -> []
    end;
takewhile2(Pred, []) when is_function(Pred, 1) -> [].

%%-------------------------------------------------------------------------------
%% @doc
%%  Returns a first N elements of a specified zlist as a zlist. Note that it does 
%%  this lazely not at once, so the N may be a quite big number without risk of 
%%  RAM hit.
%% @end
%%-------------------------------------------------------------------------------

-spec take( N :: non_neg_integer(), ZList :: zlist(T)) -> zlist(T) .

take( 0, _) ->
    [];
take( _, []) ->
    [];
take( N, [H|Tail]) ->
    new([H],fun()-> take(N-1,?EXPAND(Tail)) end).

%%-------------------------------------------------------------------------------
%% @doc
%%  Same as take/2 but "less lazy". If some first values already expanded 
%%  then 'take' logic applied immediately to them until it faces tail 
%%  function. This function may exhibit higher performance due to less calls 
%%  to tail fun.
%% @end
%%-------------------------------------------------------------------------------

-spec take2( N :: non_neg_integer(), ZList :: zlist(T)) -> zlist(T) .

take2( 0, _) ->
    [];
take2( _, []) ->
    [];
take2( N, [H|Tail]) when is_list(Tail)->
    [H| take2(N-1, Tail)];
take2( N, [H|Tail]) ->
    new([H],fun()-> take2(N-1,?EXPAND(Tail)) end).

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
    [H|expand(T)];
expand(TFun) when is_function(TFun, 0) ->
    expand(TFun()).

%%-------------------------------------------------------------------------------
%% @doc
%%  Partially unlazies a zlist. Using a specified head lengh N creates a new zlist 
%%  with first N elements available for pattern matching.
%% @end
%%-------------------------------------------------------------------------------
expand(_N, []) ->
    [];
expand(4, [_,_,_,_|_]=List) ->
    List;
expand(3, [_,_,_|_]=List) ->
    List;
expand(2, [_,_|_]=List) ->
    List;
expand(1,[_|_]=List) ->
    List;
expand(N, [H|T]) ->
    [H|expand(N-1,T)];
expand(N, TFun) when is_function(TFun, 0) ->
    expand(N, TFun()).

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
    Exp=zlists:expand(N, ZList),
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

-spec merge(ZList1 :: zlist(term()), ZList2 :: zlist(term())) -> zlist(term()).

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

-spec merge(ListOfZLists :: [zlist(term())]) -> zlist(term()).

merge([]) ->
    [];
merge([ZList1]) ->
    ZList1;
merge([ZList1,ZList2|Other]) ->
    merge([merge(ZList1, ZList2)|Other]).

%%-------------------------------------------------------------------------------
%% @doc
%%   Returns a zlist of merged zlists using specified oreding function. It is 
%%   supposed that zlists in a list are ordered using the same (specified when 
%%   merging) ordering function,
%% @end
%%-------------------------------------------------------------------------------

-spec merge_using(Fun :: fun((T, T) -> boolean()) , 
                 ListOfZLists :: [zlist(T)]) -> zlist(T).

merge_using(_Fun, []) ->
    [];
merge_using(_Fun, [ZList1]) ->
    ZList1;
merge_using(Fun, [ZList1,ZList2|Other]) ->
    merge_using(Fun, [merge(Fun, ZList1, ZList2)|Other]).

%%-------------------------------------------------------------------------------
%% @doc
%%   A lazy analog of lists:keymerge/3.
%% @end
%%-------------------------------------------------------------------------------

-spec keymerge(N, List1, List2) -> List3 when
      N :: non_neg_integer(),
      List1 :: zlist(A),
      List2 :: zlist(B),
      List3 :: zlist((A | B)).

keymerge(_N, [], ZList2) ->
    ZList2;
keymerge(_N, ZList1, []) ->
    ZList1;
keymerge(N, [H1|Tail1], [H2|_]=ZList2) when element(N,H1) =< element(N,H2) ->
    [H1| fun()-> keymerge(N,?EXPAND(Tail1),ZList2) end];
keymerge(N, ZList1, [H2|Tail2]) ->
    [H2| fun()-> keymerge(N,ZList1,?EXPAND(Tail2)) end].

%%-------------------------------------------------------------------------------
%% @doc
%%   Analog of zlists:keymerge/3, but for a multiple zlists.
%% @end
%%-------------------------------------------------------------------------------

-spec keymerge(N ::  non_neg_integer(), ListOfZLists :: [zlist(term())]) -> zlist(term()).

keymerge(_N,[]) ->
    [];
keymerge(_N,[ZList1]) ->
    ZList1;
keymerge(N,[ZList1,ZList2|Other]) ->
    keymerge(N,[keymerge(N,ZList1, ZList2)|Other]).

%%-------------------------------------------------------------------------------
%% @doc
%%   A lazy analog of lists:splitwith/2.
%% @end
%%-------------------------------------------------------------------------------

-spec splitwith(Pred, ZList) -> {List1, ZList2} when
      Pred :: fun((T) -> boolean()),
      ZList :: zlist(T),
      List1 :: [T],
      ZList2 :: zlist(T),
      T :: term().

splitwith(Pred, ZList) when is_function(Pred, 1) ->
    splitwith(Pred, ZList, []).

splitwith(_Pred, [], Acc) ->
    {lists:reverse(Acc), []};
splitwith(Pred, [H|Tail]=ZList, Acc) ->
    Satisfy=Pred(H),
    if Satisfy ->
           splitwith(Pred, ?EXPAND(Tail), [H|Acc]);
       true ->
           {lists:reverse(Acc), ZList}
    end.

%%-------------------------------------------------------------------------------
%% @doc
%%   Returns a cartesian product of two zlists as zlist.
%% @end
%%-------------------------------------------------------------------------------
-spec cartesian(ZList1 :: zlist(term()), ZList :: zlist(term())) -> zlist(list()) .  

cartesian([], _ZList2) ->
    [];
cartesian(_ZList1, []) ->
    [];
cartesian(ZList1, ZList2) ->
    generate(ZList1, fun(El)-> map(fun(Er)-> [El,Er] end, ZList2) end).

%%-------------------------------------------------------------------------------
%% @doc
%%   Returns a zlist as a result of join of a list of an ordered zlists. Actually 
%%   it is a merge join algorithm implementation.
%%   The function accepts a list of sources where each source is a binary tuple:
%%      {KeySpec, OrderedZList}, where KeySpec is an integer position of a key 
%%      in a tuples in a OrderedZList, or a function that returns a key by term 
%%      from a OrderedZList. OrderedZList is a zlist sorted accordinly to KeySpec.
%%
%% @end
%%-------------------------------------------------------------------------------

-spec join(ListOfSources :: [{KeySpec, OrderedZList}]) -> zlist(list(tuple())) 
        when KeySpec :: non_neg_integer() | fun( (E) -> Key :: term() ),
             OrderedZList :: zlist(E).

join([{_KeySpec1, ZList1}]) ->
    map(fun(E)-> [E] end, ZList1);
join([{KeySpec1, ZList1}, {KeySpec2, _}=H |Tail]=_ListOfSources) ->
    left_join(if is_integer(KeySpec1) ->
                     fun(E)-> element(KeySpec1, E) end;% Currying... 
                 is_function(KeySpec1,1) -> 
                     KeySpec1 
              end, 
              ZList1, 
              if is_integer(KeySpec2) ->
                     fun(E)-> element(KeySpec2, E) end;% Currying... 
                 is_function(KeySpec2,1) -> 
                     KeySpec2 
              end, join([H|Tail]) , false).

%%-------------------------------------------------------------------------------
%% @doc
%%   Returns a zlist as a result of join of a list of a two ordered zlists. Actually 
%%   it is a merge join algorithm implementation. There is also a possibility to 
%%   specify complement option. This option controls whether the entry ignored if 
%%   there is no match with right counterpart.
%%   
%%   Examples:
%%         42>  zlists:print(zlists:join({1, [{1,left},{2,left},{3,left}]}, {1, [{2,right},{3,right},{4,right}]},true)).      
%%         [{1,left},undefined]
%%         [{2,left},{2,right}]
%%         [{3,left},{3,right}]
%%         ok
%%         43>  zlists:print(zlists:join({1, [{1,left},{2,left},{3,left}]}, {1, [{2,right},{3,right},{4,right}]},{complement,'N/A'})).
%%         [{1,left},'N/A']
%%         [{2,left},{2,right}]
%%         [{3,left},{3,right}]
%%         ok
%%         44>  zlists:print(zlists:join({1, [{1,left},{2,left},{3,left}]}, {1, [{2,right},{3,right},{4,right}]},false)).             
%%         [{2,left},{2,right}]
%%         [{3,left},{3,right}]
%%         ok
%%
%% @end
%%-------------------------------------------------------------------------------

-spec join({KeySpec1, OrderedZList1},{KeySpec2, OrderedZList2}, ComplementOpt) -> zlist(list(tuple())) 
        when KeySpec1 :: non_neg_integer() | fun( (E) -> Key :: term() ),
             KeySpec2 :: non_neg_integer() | fun( (E) -> Key :: term() ),
             OrderedZList1 :: zlist(E),
             OrderedZList2 :: zlist(E),
             ComplementOpt :: false | true | {complement, With :: term()}.
join({KeySpec1, OrderedZList1}, {KeySpec2, OrderedZList2}, ComplementOpt) ->
    left_join(if is_integer(KeySpec1) ->
                     fun(E)-> element(KeySpec1, E) end;% Currying... 
                 is_function(KeySpec1,1) -> KeySpec1 
              end, 
              OrderedZList1, 
              if is_integer(KeySpec2) ->
                     fun(E)-> element(KeySpec2, E) end;% Currying... 
                 is_function(KeySpec2,1) -> KeySpec2 
              end, map(fun(E)-> [E] end, OrderedZList2) , ComplementOpt).

%%-------------------------------------------------------------------------------
%% @doc
%%   A lazy analog of lists:zip/2.
%% @end
%%-------------------------------------------------------------------------------

-spec zip(ZList1, ZList2) -> ZList3 when
      ZList1 :: zlist(A),
      ZList2 :: zlist(B),
      ZList3 :: zlist({A, B}).
zip([], []) ->
    [];
zip([H1|Tail1], [H2|Tail2]) ->
    new([{H1,H2}],fun()-> zip(?EXPAND(Tail1),?EXPAND(Tail2)) end).

%%-------------------------------------------------------------------------------
%% @doc
%%   Analogous to zlists:zip/2 but allow passed zlists to have a different lengths.
%% @end
%%-------------------------------------------------------------------------------

-spec ziph(ZList1, ZList2) -> ZList3 when
      ZList1 :: zlist(A),
      ZList2 :: zlist(B),
      ZList3 :: zlist({A, B}).
ziph(_ZList1, []) ->
    [];
ziph([], _ZList2) ->
    [];
ziph([H1|Tail1], [H2|Tail2]) ->
    new([{H1,H2}],fun()-> ziph(?EXPAND(Tail1),?EXPAND(Tail2)) end).

%%-------------------------------------------------------------------------------
%% @doc
%%   Given a sorted zlist returns a zlist where elements unique ('==' used).
%% @end
%%-------------------------------------------------------------------------------
-spec unique(ZList::zlist(T)) -> zlist(T).
unique([]) ->
    [];
unique([H | Tail]) ->
    [H | fun()-> unique(dropwhile(fun(E)-> E==H end, ?EXPAND(Tail))) end].

%%-------------------------------------------------------------------------------
%% @doc
%%   Given a sorted zlist and equality fun, returns a zlist where elements unique 
%%   (equality fun used). Equality fun accepts two elements and return 'true' if 
%%   they are considered as equal.
%% @end
%%-------------------------------------------------------------------------------
-spec unique(EqFun::fun((T,T)->boolean()), ZList::zlist(T)) -> zlist(T).
unique(_EqFun, []) ->
    [];
unique(EqFun, [H | Tail]) when is_function(EqFun, 2) ->
    [H | fun()-> unique(dropwhile(fun(E)-> EqFun(E,H) end, ?EXPAND(Tail))) end].

%%-------------------------------------------------------------------------------
%% @doc
%%   This function mainly for debug purposes, it traverses through entire 
%%   sequence to count a number of elements.
%% @end
%%-------------------------------------------------------------------------------

count(ZList) ->
    count(ZList, 0).

count([], N) -> N;
count([_|T], N) -> count(?EXPAND(T), N+1).

%%-------------------------------------------------------------------------------
%% @doc
%%   This function splits a dataset on chunks, apply aggregate function on each 
%%   chunk and returns aggregated values as a zlist. To split dataset on chunks 
%%   two entities used: zlist of partition markers and a function which decide
%%   whether the element of a dataset belongs to the part marked by marker.
%%
%%   Example:
%%      Markers=[1,11,21,31,41,51],
%%      Values=lists:seq(15, 35),
%%      AggFun=fun(L)-> L end,
%%      PredFun=fun(Marker,Value)-> Value < Marker end,
%%      zlists:expand(
%%         zlists:aggregate(
%%             AggFun, PredFun, Markers, Values)).
%%
%%     The result is:
%%     [[],[],
%%      [15,16,17,18,19,20],
%%      [21,22,23,24,25,26,27,28,29,30],
%%      [31,32,33,34,35],[]]
%% @end
%%-------------------------------------------------------------------------------
-spec aggregate(AggFun :: fun( ([T]) -> T1 ), 
                PredFun :: fun( (M,T) -> boolean() ), 
                MarkerZList :: zlist(M), 
                DataZList :: zlist(T) ) -> zlist(T1).

aggregate(_AggFun, _PredFun, [], _DataZList) ->
    [];
aggregate(AggFun, PredFun, [Marker|MTail]=_MarkerZList, DataZList) 
  when is_function(AggFun, 1), is_function(PredFun, 2) ->
    {Ready,DataZList1}=zlists:splitwith(fun(E)-> PredFun(Marker,E) end, DataZList),
    [AggFun(Ready) | fun()-> aggregate(AggFun, PredFun, ?EXPAND(MTail), DataZList1) end].


%%-------------------------------------------------------------------------------
%% @doc
%%  Debug function to print zlists. Do not use it for infinit zlists.
%% @end
%%-------------------------------------------------------------------------------
print(L) ->
    foreach(fun(E)-> io:format("~p~n",[E]) end, L).

print(SkipN, PrintN ,ZList) ->
    print(take(PrintN, drop(SkipN,ZList))).

%%
%% Local Functions
%%

left_join(_KeyFun1, ZList1, _KeyFun2, [], ComplementOpt) ->
    case ComplementOpt of
        {complement, With} -> left_cartesian(ZList1, [[With]]);
        true               -> left_cartesian(ZList1, [[undefined]]);
        false              -> []
    end;
left_join(_KeyFun1, [], _KeyFun2, _ZList2, _ComplementOpt) ->
    [];
left_join(KeyFun1, [H1|Tail1]=ZList1, KeyFun2, [[H2|_]|Tail2]=ZList2, ComplementOpt) 
  when is_function(KeyFun1,1), is_function(KeyFun2,1) ->
    K1=KeyFun1(H1),
    K2=KeyFun2(H2),
    if K1 < K2 ->
           TF=fun()->left_join(KeyFun1, ?EXPAND(Tail1), KeyFun2, ZList2, ComplementOpt) end,
           case ComplementOpt of
               false              ->
                   left_join(KeyFun1, ?EXPAND(Tail1), KeyFun2, ZList2, ComplementOpt);
               {complement, With} ->
                   new(left_cartesian([H1], [[With]]),     TF);
               true               ->
                   new(left_cartesian([H1], [[undefined]]),TF)
           end;
       K1 > K2 ->
           left_join(KeyFun1, ZList1, KeyFun2, ?EXPAND(Tail2), ComplementOpt) ;
       true ->
           Pred1=fun(E) -> KeyFun1(E) == K1 end,
           {K1List, ZList11}=splitwith(Pred1, ZList1),
           Pred2=fun([E|_]) -> KeyFun2(E) == K2 end,
           {K2List, ZList21}=splitwith(Pred2, ZList2),
           new(left_cartesian(K1List, K2List), fun()-> left_join(KeyFun1, ZList11, KeyFun2, ZList21, ComplementOpt) end)
    end.

left_cartesian([El], [Er]) -> % Optimize highly frequent case
    [[El|Er]];
left_cartesian([], _ZList2) ->
    [];
left_cartesian(_ZList1, []) ->
    [];
left_cartesian(ZList1, ZList2) ->
    generate(ZList1, fun(El)-> map(fun(Er)-> [El|Er] end, ZList2) end).

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
