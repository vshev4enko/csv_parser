%%%-------------------------------------------------------------------
%%% @author vshev4enko
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Aug 2018 15:04
%%%-------------------------------------------------------------------
-module(simple_parser).
-author("vshev4enko").

%% API
-export([parse_bin/1, parse/1]).


%%%-------------------------------------------------------------------
%%% Example
%%% simple_csv_parser:parse("example.csv").
%%%-------------------------------------------------------------------
parse(File) ->
  case file:read_file(File) of
    {ok, Bin} ->
      parse_bin(Bin);
    Res -> Res
  end.

parse_bin(Bin) when is_binary(Bin) ->
  {ok, Tail, Headers} = parse_headers(Bin),
  {ok, Body} = parse_body(Tail),
  io:format("\nHEADERS : ~p\n", [Headers]),
  io:format("\nBODY : ~p\n", [Body]).

parse_headers(Bin) ->
  parse_headers(Bin, <<>>, []).
parse_headers(<<$\r, Rest/binary>>, Buf, Acc) ->
  parse_headers(Rest, Buf, Acc);
parse_headers(<<$\n, Rest/binary>>, Buf, Acc) ->
  {ok, Rest, lists:reverse([Buf | Acc])};
parse_headers(<<$,, Rest/binary>>, Buf, Acc) ->
  parse_headers(Rest, <<>>, [Buf | Acc]);
parse_headers(<<Char, Rest/binary>>, Buf, Acc) ->
  parse_headers(Rest, <<Buf/binary, Char>>, Acc).

parse_body(Body) ->
  parse_body(Body, <<>>, [], []).

parse_body(<<>>, Buf, Line, Acc) ->
  {ok, lists:reverse([lists:reverse([Buf | Line]) | Acc])};
parse_body(<<$\", Rest/binary>>, Buf, Line, Acc) ->
  parse_body(Rest, Buf, Line, Acc);
parse_body(<<$\r, Rest/binary>>, Buf, Line, Acc) ->
  parse_body(Rest, Buf, Line, Acc);
parse_body(<<$ , Rest/binary>>, Buf, Line, Acc) ->
  parse_body(Rest, Buf, Line, Acc);
parse_body(<<$\n, Rest/binary>>, Buf, Line, Acc) ->
  parse_body(Rest, <<>>, [], [lists:reverse([Buf | Line]) | Acc]);
parse_body(<<$,, Rest/binary>>, Buf, Line, Acc) ->
  parse_body(Rest, <<>>, [Buf | Line], Acc);
parse_body(<<Char, Rest/binary>>, Buf, Line, Acc) ->
  parse_body(Rest, <<Buf/binary, Char>>, Line, Acc).