%%%-------------------------------------------------------------------
%%% @author vshev4enko
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Aug 2018 23:20
%%%-------------------------------------------------------------------
-module(csv_parser).
-author("vshev4enko").

%% API
-export([parse/1, timeit/2, parse_body/1]).

%%init() ->
%%  ets:new(?MODULE, [set, public,named_table]).
%%
%%save_to_ets(Proplist) ->
%%  ok.


%%%-------------------------------------------------------------------
%%% parser:timeit(fun parser:parse/1, ["example.csv"]).
%%%-------------------------------------------------------------------
timeit(F, Args) ->
  {_, S, MS} = os:timestamp(),
  erlang:apply(F, Args),
  {_, S2, MS2} = os:timestamp(),
  {S2 - S, MS2 - MS}.


parse(File) ->
  case file:read_file(File) of
    {ok, Bin} ->
      parse_bin(Bin);
    Res ->
      Res
  end.

parse_bin(Bin) when is_binary(Bin) ->
  {ok, _Body, Headers} = parse_headers(Bin),
  {ok, _Res} = parse_body(_Body),
  io:format("\nheaders : ~p\n", [Headers]).

parse_headers(Bin) ->
  parse_headers(Bin, <<>>, []).
parse_headers(<<$\r, Bin/binary>>, Buf, Acc) ->
  {ok, Bin, lists:reverse([Buf | Acc])};
parse_headers(<<$\n, Bin/binary>>, Buf, Acc) ->
  {ok, Bin, lists:reverse([Buf | Acc])};
parse_headers(<<$,, Bin/binary>>, Buf, Acc) ->
  parse_headers(Bin, <<>>, [Buf | Acc]);
parse_headers(<<X, Bin/binary>>, Buf, Acc) ->
  parse_headers(Bin, <<Buf/binary, X>>, Acc).

parse_body(Body) ->
  parse_body(Body, <<>>, [], []).

parse_body(<<>>, Buf, Acc, Result) ->
  {ok, [[Buf | Acc] | Result]};
parse_body(<<$\r, Rest/binary>>, Buf, Acc, Result) ->
  parse_body(Rest, <<>>, [], [[Buf | Acc] | Result]);
parse_body(<<$\n, Rest/binary>>, Buf, Acc, Result) ->
  parse_body(Rest, <<>>, [], [[Buf | Acc] | Result]);
parse_body(<<$,, Rest/binary>>, Buf, Acc, Result) ->
  parse_body(Rest, <<>>, [Buf | Acc], Result);
parse_body(<<X, Rest/binary>>, Buf, Acc, Result) ->
  parse_body(Rest, <<Buf/binary, X>>, Acc, Result).

