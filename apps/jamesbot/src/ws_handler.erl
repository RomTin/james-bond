-module(ws_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, Opts) ->
  io:format("system: websocket connection established~n"),
  {cowboy_websocket, Req, Opts}.

websocket_handle({text, Msg}, Req, State) ->
  io:format("system: request received~n"),
  jamesbot_sup:start_worker(self(), binary_to_list(Msg)),
  {reply, {text, << "request sent: ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({text, Msg}, Req, State) ->
  self() ! list_to_binary(Msg),
  {ok, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.
