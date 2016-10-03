%%%-------------------------------------------------------------------
%%% @author Jakub Stefanski
%%% @copyright (C) 2016, Jakub Stefanski
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rabbit_wh_amqp).

-behaviour(gen_server).

-include("rabbit_wh.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/0,
         get_components/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {
         channel,
         request_exchange :: binary(),
         response_exchange :: binary()
        }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Gets the list of components
%% @end
%%--------------------------------------------------------------------
get_components() ->
    gen_server:call(?MODULE, get_components).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Connection} = amqp_connection:start(#amqp_params_direct{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {ok, RequestExchange} = application:get_env(rabbitmq_web_health, request_exchange),
    {ok, ResponseExchange} = application:get_env(rabbitmq_web_health, response_exchange),

    rabbit_log:info("Started WebHealth AMQP process"),
    {ok, #state{
            channel = Channel,
            request_exchange = RequestExchange,
            response_exchange = ResponseExchange}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_components, _From, #state{request_exchange = RequestExchange} = State) ->
    Components = [get_vhost_components(VirtualHost, RequestExchange) ||
                  VirtualHost <- rabbit_vhost:list()],
    Reply = {ok, lists:flatten(Components)},
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, unknown_command, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{channel = Channel}) ->
    amqp_channel:call(Channel, #'channel.close'{}),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get components for specified Virtual Host
%% @end
%%--------------------------------------------------------------------
get_vhost_components(VirtualHost, RequestExchange) ->
    Resource = rabbit_misc:r(VirtualHost, exchange, RequestExchange),
    [#component{vhost = VirtualHost, name = Key} ||
     #binding{key = Key} <- rabbit_binding:list_for_source(Resource)].
