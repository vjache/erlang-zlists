%%%-------------------------------------------------------------------
%%% @author <vjache@gmail.com>
%%% @copyright (C) 2011, Vyacheslav Vorobyov
%%% @doc
%%% TODO: Add description
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
		 print/1]).

-define(EXPAND(Tail), if is_function(Tail, 0) -> Tail(); true -> Tail end).

-type zlist(T) :: [T] | maybe_improper_list(T, fun( () -> zlist(T) ) ) .
-type zlist() :: zlist(any()) .

-export_type([zlist/1, zlist/0]).

%%
%% API Functions
%%
new([],Fun) when is_function(Fun, 0) ->
    Fun();
new([E],Fun) when is_function(Fun, 0) ->
    [E|Fun];
new([H|Tail],Fun) when is_function(Fun, 0) ->
    [H|fun() -> new(?EXPAND(Tail), Fun) end].

generate([], _GeneratorFun) ->
    [];
generate([H|Tail], GeneratorFun) when is_function(GeneratorFun, 1) ->
    new(GeneratorFun(H),
        fun()-> generate(?EXPAND(Tail),GeneratorFun) end).

-spec foreach(Fun, List) -> ok when
      Fun :: fun((Elem :: T) -> term()),
      List :: [T],
      T :: term().

foreach(F, [Hd|Tail]) ->
    F(Hd),
	foreach(F, ?EXPAND(Tail));
foreach(F, []) when is_function(F, 1) -> ok.

-spec foldl(Fun, Acc0, List) -> Acc1 when
      Fun :: fun((Elem :: T, AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      List :: [T],
      T :: term().

foldl(F, Accu, [Hd|Tail]) ->
    foldl(F, F(Hd, Accu), ?EXPAND(Tail));
foldl(F, Accu, []) when is_function(F, 2) -> Accu.

-spec map(Fun, List1) -> List2 when
      Fun :: fun((A) -> B),
      List1 :: [A],
      List2 :: [B],
      A :: term(),
      B :: term().

map(F, [H|T]) ->
    [F(H)|fun()-> map(F, ?EXPAND(T)) end];
map(F, []) when is_function(F, 1) -> [].

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

-spec dropwhile(Pred, List1) -> List2 when
      Pred :: fun((Elem :: T) -> boolean()),
      List1 :: [T],
      List2 :: [T],
      T :: term().

dropwhile(Pred, [Hd|Tail]=Rest) ->
    case Pred(Hd) of
	true -> dropwhile(Pred, ?EXPAND(Tail));
	false -> Rest
    end;
dropwhile(Pred, []) when is_function(Pred, 1) -> [].

-spec filter(Pred, List1) -> List2 when
      Pred :: fun((Elem :: T) -> boolean()),
      List1 :: [T],
      List2 :: [T],
      T :: term().

filter(Pred, List) when is_function(Pred, 1) ->
	Pred1=fun(E)-> not Pred(E) end,
    case dropwhile(Pred1, List) of
		[] -> [];
		[_]=R -> R;
		[H|T] -> [H| fun()-> filter(Pred, ?EXPAND(T)) end]
	end.

expand([]) ->
	[];
expand([H|T]) ->
	[H|expand(?EXPAND(T))].

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

-spec append(ZListOfZLists:: zlist(zlist(T))) -> zlist(T) when T :: term().

append([]) ->
    [];
append([ZList]) ->
    ZList;
append([ZList | OtherZLists]) ->
    new(ZList, fun()-> append(?EXPAND(OtherZLists)) end).

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

print(L) ->
	foreach(fun(E)-> io:format("~p~n",[E]) end, L).
%%
%% Local Functions
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
