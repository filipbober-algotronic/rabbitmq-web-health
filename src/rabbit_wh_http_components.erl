%%%-------------------------------------------------------------------
%%% @author Jakub Stefanski
%%% @copyright (C) 2016, Jakub Stefanski
%%% @doc
%%% HTTP /api/components handler
%%% @end
%%%-------------------------------------------------------------------
-module(rabbit_wh_http_components).

-include("rabbit_wh.hrl").

%% Cowboy REST handler callbacks
-export([init/3,
         content_types_provided/2,
         components_to_json/2]).

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
init(_Type, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Specifies content types that are provided
%%
%% @spec content_types_provided(Req, State) -> {[{binary() | {binary(), binary(), '*' |
%%                                             [{binary(), binary()}]}, atom()}], Req, State} |
%%                                             {stop, Req, State}
%%      Req = cowboy_req:req()
%%      State = any()
%% @end
%%--------------------------------------------------------------------
content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, components_to_json}
     ], Req, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles the JSON request
%% @end
%%--------------------------------------------------------------------
components_to_json(Req, State) ->
    {ReqVirtualHost, NewReq} = cowboy_req:binding(vhost, Req),
    {ok, Components} = case ReqVirtualHost of
                           undefined ->
                               rabbit_wh_amqp:list_components();
                           _ ->
                               rabbit_wh_amqp:list_components_for_vhost(ReqVirtualHost)
                       end,
    Serializable = [[{<<"vhost">>, VirtualHost}, {<<"name">>, Name}] ||
                    #component{vhost = VirtualHost, name = Name} <- Components],
    {ok, Body} = jsone:try_encode([{<<"components">>, Serializable}]),
    {Body, NewReq, State}.
