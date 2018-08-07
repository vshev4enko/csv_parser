%%%-------------------------------------------------------------------
%%% @author vshev4enko
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Aug 2018 23:20
%%%-------------------------------------------------------------------
-module(parser).
-author("vshev4enko").

%% API
-export([parse/1]).


parse(File) ->
  case file:read_file(File) of
    {ok, Bin} ->
      parse_bin(Bin);
    Res ->
      Res
  end.

parse_bin(Bin) when is_binary(Bin) ->
  {ok, Body, Headers} = parse_headers(Bin),
  Res = parse_body(Headers, Body),
  io:format("\nheaders : ~p", [Headers]),
  Res.

parse_headers(Bin) ->
  parse_headers(Bin, <<>>, []).
parse_headers(<<"\r"/utf8, Rest/binary>>, IAcc, RAcc) ->
  {ok, Rest, lists:reverse([IAcc|RAcc])};
parse_headers(<<","/utf8, Rest/binary>>, IAcc, RAcc) ->
  parse_headers(Rest, <<>>, [IAcc|RAcc]);
parse_headers(<<H/utf8, Rest/binary>>, IAcc, RAcc) ->
  parse_headers(Rest, <<IAcc/binary, H>>, RAcc).

parse_body(Headers, Body) ->
  parse_body(Headers, Headers, Body, <<>>, [], []).

parse_body(_, [H|_], <<>>, _IAcc, _RAcc, Result) ->
  [[{H, _IAcc}|_RAcc]|Result];
parse_body(Headers, [H|_], <<"\r"/utf8, Rest/binary>>, IAcc, RAcc, Result) ->
  parse_body(Headers, Headers, Rest, <<>>, [], [[{H,IAcc}|RAcc]|Result]);
parse_body(Headers, [H|T], <<","/utf8, Rest/binary>>, IAcc, RAcc, Result) ->
  parse_body(Headers, T, Rest, <<>>, [{H, IAcc}|RAcc], Result);
parse_body(Headers, Head, <<H/utf8, Rest/binary>>, IAcc, RAcc, Result) ->
  parse_body(Headers, Head, Rest, <<IAcc/binary, H>>, RAcc, Result).
