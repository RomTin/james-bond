%%%-------------------------------------------------------------------
%% @doc jamesbot worker process definition
%% @end
%%%-------------------------------------------------------------------

-module(jamesbot_srv).

-include("config.hrl").
-behaviour(gen_server).

%% Application callbacks
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([post/1, delete/1]).
-record(state, {handler, text, id_str, sent, deleted}).

%%====================================================================
%% initialization, termination
%%====================================================================

start_link({jamesbot, {?MODULE, start_link, [HandlerPid, Text]}, transient, _, worker, [?MODULE]}) ->
  gen_server:start_link(?MODULE, {HandlerPid, Text}, []);
start_link(_) ->
  io:format("system: invalid child spec~n"),
  erorr.

init({HandlerPid, Text}) ->
  self() ! send_tweet,
  {ok, #state{handler = HandlerPid, text = Text, sent = false, deleted = false} }.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
  ok.

%%====================================================================
%% callback functions
%%====================================================================

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Info, State) ->
  {noreply, State}.

handle_info(_Op, State = #state{handler = HandlerPid, text = Text, id_str = Id, sent = Sflag, deleted = Dflag}) ->
  case {Sflag, Dflag} of
    {false, _} ->
      Res = post(Text ++ binary_to_list(?SUFFIX)),
      NewId = find_id(Res),
      self() ! delete_tweet,
      {noreply, State#state{id_str = NewId, sent = true}};
    {true, false} ->
      Res = timer:apply_after(?INTERVAL * 1000, jamesbot_srv, delete, [Id]),
      _ = parse_response(Res),
      {noreply, State#state{deleted = true} };
    {true, true} ->
      {stop, normal, #state{}}
  end.

code_change(_Old, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% public functions
%%====================================================================

post(Text) ->
  ssl:start(),
  ApiKey = {?CK, ?CS, hmac_sha1},
  URL = "https://api.twitter.com/1.1/statuses/update.json",
  Response = oauth:post(URL, [{"status", Text}], ApiKey, ?AT, ?AS).

delete(TweetIdStr) ->
  ssl:start(),
  ApiKey = {?CK, ?CS, hmac_sha1},
  URL = "https://api.twitter.com/1.1/statuses/destroy/" ++ TweetIdStr ++ ".json",
  Response = oauth:post(URL, [{"id", TweetIdStr}], ApiKey, ?AT, ?AS).

%%====================================================================
%% private functions
%%====================================================================

find_id(Response) ->
  BodyJson = jsx:decode(list_to_binary(parse_response(Response))),
  integer_to_list(proplists:get_value(<<"id">>, BodyJson)).

parse_response({ok, {{_, 200, _}, _, Body}}) -> Body;
parse_response({ok, {{_, _, _}, _, _}})-> error.
