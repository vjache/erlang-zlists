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
-export([read/2, 
         expand_binary/2, 
         as_berps/2, 
         read_berps/1,
         read_berps/2,
         open_berps/1, 
         open_berps/2, 
         close_on_eof/1, 
         close_on_eof/2,
         consult_berps/1,
         write_berps/2,
         dump_berps/2]).

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

expand_binary([Bin|ZTail]=BinZList,Sz) when byte_size(Bin) >= Sz ->
    if byte_size(Bin) == Sz -> BinZList;
       true                 ->
           <<Chunk:Sz/binary,BTail/binary>> = Bin,
           [Chunk,BTail|ZTail]
    end;
expand_binary([Bin],Sz) when byte_size(Bin) < Sz ->
    [Bin];
expand_binary([Bin|ZTail],Sz) ->
    case zlists:expand(1,ZTail) of
        [H|ZTail1]  -> expand_binary([<<Bin/binary,H/binary>>|ZTail1],Sz);
        []          -> [Bin]
    end.

%%-------------------------------------------------------------------------------
%% @doc
%%  Takes a Z-List of bytes representing BERPs and returns Z-List of terms. An 
%%  input Z-List either Z-List of binaries or Z-List of integers B in [0..255]. 
%% @end
%%-------------------------------------------------------------------------------
-spec as_berps(BytesZList :: zlists:zlist(binary()) | zlists:zlist(non_neg_integer()), 
               Opts       :: [safe]) -> zlists:zlist(term()).
as_berps([],_Opts)   -> [];
as_berps([Bin|_]=BinZList,Opts) 
  when is_binary(Bin)  ->
    case expand_binary(BinZList, 4) of
        [<<0 :32/unsigned-integer>>] -> [];
        [<<Sz:32/unsigned-integer>>|T] ->
            [<<Bert:Sz/binary>>|T1]=expand_binary(T, Sz),
            [binary_to_term(Bert, Opts)|fun()-> as_berps(zlists:expand(1,T1),Opts) end]
    end;
as_berps(ZList,Opts) ->
    case zlists:expand(4,ZList) of
        []-> throw({dbg, ZList});
        [0,0,0,0] -> [];
        [A,B,C,D|T] -> 
            <<Sz:32/unsigned-integer>> = <<A,B,C,D>>,
            {Data,T1}=zlists:scroll(Sz,T),
            [binary_to_term(list_to_binary(Data),Opts)|fun()-> as_berps(zlists:expand(1,T1),Opts) end]
    end.

%%-------------------------------------------------------------------------------
%% @doc
%%  Equivalent to  read_berps(Io, []).
%% @end
%%-------------------------------------------------------------------------------
-spec read_berps(IoDevice :: file:io_device()) -> zlists:zlist(term()).
read_berps(Io) ->
    read_berps(Io, []).

%%-------------------------------------------------------------------------------
%% @doc
%%  Returns a Z-List of terms contained in an IO device as BERPs. 
%% @end
%%-------------------------------------------------------------------------------
-spec read_berps(IoDevice :: file:io_device(), 
                 Opts     :: [safe]) -> zlists:zlist(term()).
read_berps(Io, Opts) ->
    case file:read(Io, 4) of
        eof -> [];
        {ok,<<Sz:32/unsigned-integer>>} ->
            {ok,<<Bert:Sz/binary>>}=file:read(Io, Sz),
            [binary_to_term(Bert, Opts)|fun()-> read_berps(Io) end]
    end.

%%-------------------------------------------------------------------------------
%% @doc
%%  Just a helper function to open a BERP file as a Z-List at one op. 
%%  Checks if suffix is ".gz" to decide enflate or not.
%% @end
%%-------------------------------------------------------------------------------
-spec open_berps(Filename :: file:filename()) -> {file:io_device(), zlists:zlist(term())}.
open_berps(Filename) ->
    IsGZip = is_gz(Filename),
    open_berps(Filename, IsGZip).

%%-------------------------------------------------------------------------------
%% @doc
%%  Just a helper function to open a BERP file as a Z-List at one op.
%% @end
%%-------------------------------------------------------------------------------
-spec open_berps(Filename :: file:filename(), 
                 IsGZip   :: boolean()) -> {file:io_device(), zlists:zlist(term())}.
open_berps(Filename, IsGZip) ->
    {ok,Io}=file:open(
              Filename, 
              [read,raw,
               binary,read_ahead | if IsGZip-> [compressed]; true -> [] end ]),
    {Io, read_berps(Io)}.

%%-------------------------------------------------------------------------------
%% @doc
%%  Reads entire file into one binary, and wraps it with Z-List that will parse 
%%  it as a sequence of BERPs.
%% @end
%%-------------------------------------------------------------------------------
-spec consult_berps(Filename :: file:filename()) -> zlists:zlist(term()).
consult_berps(Filename) ->
    {ok,Data}=file:read_file(Filename),
    IsGZip=is_gz(Filename),
    IData= if IsGZip -> zlib:gunzip(Data); true -> Data end,
    as_berps([IData], []).    

%%-------------------------------------------------------------------------------
%% @doc
%%  Writes a terms passed as Z-List to IO device using BERP format.
%% @end
%%-------------------------------------------------------------------------------
-spec write_berps(IoDevice :: file:io_device(), Terms :: zlists:zlist(term()) ) -> ok.
write_berps(Io, ZList) ->
    zlists:foreach(
      fun(Term) -> 
              Bin = term_to_binary(Term),
              Sz  = byte_size(Bin),
              ok=file:write(Io, <<Sz:32/unsigned-integer,Bin/binary>>) 
      end,
      ZList).
%%-------------------------------------------------------------------------------
%% @doc
%%  Writes a terms passed as Z-List to file using BERP format.
%% @end
%%-------------------------------------------------------------------------------
-spec dump_berps(Filename :: file:filename(), Terms :: zlists:zlist(term()) ) -> ok.
dump_berps(Filename, ZList) ->
    IsGZip = is_gz(Filename),
    {ok,Io}=file:open(Filename, 
              [write,raw,
               binary,delayed_write | if IsGZip-> [compressed]; true -> [] end ]),
    try write_berps(Io, ZList)
    after
        file:close(Io)
    end.

close_on_eof({Io, ZList}) ->
    close_on_eof(Io, ZList).

close_on_eof(Io, ZList) ->
    zlists:new(ZList, fun()-> file:close(Io),[] end).

%%
%% Local Functions
%%

is_gz(Filename) -> 
    lists:suffix(
      ".gz", 
      if is_list(Filename)   -> Filename;
         is_atom(Filename)   -> atom_to_list(Filename); 
         is_binary(Filename) -> binary_to_list(Filename) 
      end).
