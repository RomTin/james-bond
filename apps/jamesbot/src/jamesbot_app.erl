%%%-------------------------------------------------------------------
%% @doc jamesbot public API
%% @end
%%%-------------------------------------------------------------------

-module(jamesbot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           %{"/", cowboy_static, {priv_file, websocket, "index.html"}},
                                           {"/ws", ws_handler, []}
                                          ]}
                                   ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 10080}],
                              [{env, [{dispatch, Dispatch}]}]),
  jamesbot_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
