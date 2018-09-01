%%%-------------------------------------------------------------------
%%% @author vshev4enko
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Aug 2018 05:55
%%%-------------------------------------------------------------------
-module(csv_parser).
-author("vshev4enko").

%% API
-export([process_csv/3]).

-record(state, {
  current_line = [],
  buffer = <<>>,
  process_fun,
  process_fun_state
}).

process_csv(IoDevice, ProcessFun, InitFunState) ->
  InitState = #state{
    process_fun = ProcessFun,
    process_fun_state = InitFunState
  },
  stream_from_io_device(IoDevice, InitState).

%%%-------------------------------------------------------------------
%%% Internal function
%%%-------------------------------------------------------------------
stream_from_io_device(IoDevice, InitState) ->
  IoDevIterator =
    fun(Io) ->
      {io:get_chars(Io, "", 1), Io}
    end,
  iterate(IoDevIterator, IoDevice, InitState).

iterate(IteratorFun, IoSource, State) ->
  {FirstChar, NewIoSource} = IteratorFun(IoSource),
  iterate(IteratorFun, NewIoSource, State, FirstChar).

iterate(_, _, State, eof) ->
  end_parse(State);
iterate(IteratorFun, IoSource, State, Char) ->
  NewState = process_char(Char, State),
  {FirstChar, NewIoSource} = IteratorFun(IoSource),
  iterate(IteratorFun, NewIoSource, NewState, FirstChar).

end_parse(
    #state{
      current_line = Line,
      buffer = Buffer,
      process_fun = ProcessFun,
      process_fun_state = ProcessFunState
    }) ->
  NewLine = lists:reverse([Buffer | Line]),
  {ok, process_fun(ProcessFun, {eof, NewLine}, ProcessFunState)}.

process_char(<<$">>, State) ->
  State;
process_char(<<$\r>>, State) ->
  State;
process_char(<<$,>>,
    #state{
      current_line = Line,
      buffer = Buffer
    } = State) ->
  State#state{
    buffer = <<>>,
    current_line = [Buffer | Line]
  };
process_char(<<$\n>>,
    #state{
      current_line = Line,
      buffer = Buffer,
      process_fun = ProcessFun,
      process_fun_state = ProcessFunState
    } = State) ->
  NewLine = lists:reverse([Buffer | Line]),
  NewFunState = process_fun(ProcessFun, {newline, NewLine}, ProcessFunState),
  State#state{
    current_line = [],
    buffer = <<>>,
    process_fun_state = NewFunState
  };
process_char(Char, #state{buffer = Buffer} = State) when is_binary(Char) ->
  State#state{
    buffer = <<Buffer/binary, Char/binary>>
  }.

process_fun(ProcessFun, Line, State) ->
  ProcessFun(Line, State).