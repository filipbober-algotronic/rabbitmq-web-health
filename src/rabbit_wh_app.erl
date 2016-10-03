%%%-------------------------------------------------------------------
%%% @author Jakub Stefanski
%%% @copyright (C) 2016, Jakub Stefanski
%%% @doc
%%% RabbitMQ Plugin to check health of components via HTTP
%%% @end
%%%-------------------------------------------------------------------
-module(rabbit_wh_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Constants
-define(CONTEXT, rabbit_wh_app).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    {ok, Listener} = application:get_env(rabbitmq_web_health, listener),
    register_context(Listener),
    rabbit_log:info("WebHealth plugin started. Port: ~w", [port(Listener)]),
    rabbit_wh_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    unregister_context(),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

register_context(Listener) ->
    Routes = [
              {'_', [
                     {"/api/components[/:vhost]", rabbit_wh_http_components, []},
                     {"/api/health", rabbit_wh_http_health, []}
                    ]}
             ],
    rabbit_web_dispatch:register_context_handler(
      ?CONTEXT, Listener, "",
      cowboy_router:compile(Routes),
      "HTTP interface to check components health").

unregister_context() ->
    rabbit_web_dispatch:unregister_context(?CONTEXT).

port(Listener) ->
    proplists:get_value(port, Listener).
