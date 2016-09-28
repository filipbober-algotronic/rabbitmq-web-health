%%%-------------------------------------------------------------------
%%% @author Jakub Stefanski
%%% @copyright (C) 2016, Jakub Stefanski
%%% @doc
%%% HTTP /api/health handler
%%% @end
%%%-------------------------------------------------------------------
-module(rabbit_wh_http_health).

%% Cowboy handler callbacks
-export([init/3,
         handle/2,
         terminate/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the handler
%% @end
%%--------------------------------------------------------------------
init(_Type, Req, []) ->
    {ok, Req, no_state}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the request
%% @end
%%--------------------------------------------------------------------
handle(Req, State) ->
    Headers = [
               {<<"content-type">>, <<"application/json">>}
              ],
    Content = <<"{\"healthy\": true}">>,
    {ok, Res} = cowboy_req:reply(200, Headers, Content, Req),
    {ok, Res, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Clean up
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _Req, _State) ->
    ok.

