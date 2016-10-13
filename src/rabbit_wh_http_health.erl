%%%-------------------------------------------------------------------
%%% @author Jakub Stefanski
%%% @copyright (C) 2016, Jakub Stefanski
%%% @doc
%%% HTTP /api/health handler
%%% @end
%%%-------------------------------------------------------------------
-module(rabbit_wh_http_health).
-behavior(cowboy_http_handler).

%% Cowboy HTTP handler callbacks
-export([init/3,
         handle/2,
         terminate/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the request handler
%%
%% @spec init(Type, Req, State) -> {ok, Req, State} |
%%                                 {loop, Req, State} |
%%                                 {loop, Req, State, hibernate} |
%%                                 {loop, Req, State, timeout()} |
%%                                 {loop, Req, State, timeout(), hibernate} |
%%                                 {shutdown, Req, State} |
%%                                 {upgrade, protocol, module()} |
%%                                 {upgrade, protocol, module(), Req, any()}
%%      Type = {atom(), http}
%%      Req = cowboy_req:req()
%%      State = any()
%% @end
%%--------------------------------------------------------------------
init(_Type, Req, []) ->
    {ok, Req, no_state}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the request
%%
%% @spec handle(Req, State) -> {ok, Req, State}
%%      Req = cowboy_req:req()
%%      State = any()
%% @end
%%--------------------------------------------------------------------
handle(Req, State) ->
    {ReqVirtualHost, NewReq1} = cowboy_req:binding(vhost, Req),
    {ReqComponent, NewReq2} = cowboy_req:binding(component, NewReq1),
    {ReqContentType, NewReq3} = cowboy_req:header(<<"content-type">>, NewReq2, <<"application/json">>),
    rabbit_wh_amqp:check_health(ReqVirtualHost, ReqComponent, ReqContentType),
    Headers = [
               {<<"content-type">>, ReqContentType}
              ],
    Content = <<"{\"healthy\": true}">>,
    {ok, Res} = cowboy_req:reply(200, Headers, Content, NewReq3),
    {ok, Res, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Clean up
%%
%% @spec terminate(Reason, Req, State) -> ok
%%      Reason = {normal, shutdown} |
%%               {normal, timeout} | %% Only occurs in loop handlers.
%%               {error, closed} | %% Only occurs in loop handlers.
%%               {error, overflow} | %% Only occurs in loop handlers.
%%               {error, atom()}
%%      Req = cowboy_req:req()
%%      State = any()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _Req, _State) ->
    ok.

