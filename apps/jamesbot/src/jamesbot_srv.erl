%%%-------------------------------------------------------------------
%% @doc jamesbot worker process definition
%% @end
%%%-------------------------------------------------------------------

-module(jamesbot_srv).

-include("config.hrl").
-behaviour(gen_server).

%% Application callbacks
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-compile(export_all).
%%====================================================================
%% initialization, termination
%%====================================================================


%%--------------------------------------------------------------------


%%====================================================================
%% callback functions
%%====================================================================


%%====================================================================
%% Internal functions
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

find_id(Response) ->
  BodyJson = jsx:decode(list_to_binary(parse_response(Response))),
  integer_to_list(proplists:get_value(<<"id">>, BodyJson)).

parse_response({ok, {{_, 200, _}, _, Body}}) -> Body;
parse_response({ok, {{_, _, _}, _, _}})-> error.
