%%%-------------------------------------------------------------------
%% @doc jamesbot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(jamesbot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_worker/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(RESTART, {simple_one_for_one, 1, 1000}).
%% Args must be a list
-define(CHILD_SPEC(Args), {jamesbot, {jamesbot_srv, start_link, Args}, transient, 2000, worker, [jamesbot_srv]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_worker(HandlerPid, Text) ->
  io:format("system: worker will be initialized~n"),
  supervisor:start_child(?SERVER, [?CHILD_SPEC([HandlerPid, Text])]).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, { ?RESTART, [?CHILD_SPEC([])]} }.

%%====================================================================
%% Internal functions
%%====================================================================
