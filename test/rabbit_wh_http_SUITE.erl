%%%-------------------------------------------------------------------
%%% @author Jakub Stefanski
%%% @copyright (C) 2016, Jakub Stefanski
%%% @doc
%%% RabbitMQ Web Health plugin HTTP modules tests
%%% @end
%%%-------------------------------------------------------------------
-module(rabbit_wh_http_SUITE).

%% API
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([
         no_components_tests/1,
         all_components_tests/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(HTTPC_OPTS, [{version, "HTTP/1.0"}, {autoredirect, false}]).
-define(REQ_EXCHANGE, <<"GetHealthRequest">>).

all() ->
    [
     {group, non_parallel_tests}
    ].

groups() ->
    [
     {non_parallel_tests, [], [
                               no_components_tests,
                               all_components_tests
                              ]}

    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    inets:start(),
    NewConfig = rabbit_ct_helpers:set_config(Config, [{rmq_nodename_suffix, ?MODULE}]),
    rabbit_ct_helpers:run_setup_steps(NewConfig,
                                      rabbit_ct_broker_helpers:setup_steps() ++
                                      rabbit_ct_client_helpers:setup_steps()).

end_per_suite(Config) ->
    rabbit_ct_helpers:run_teardown_steps(Config,
                                         rabbit_ct_client_helpers:teardown_steps() ++
                                         rabbit_ct_broker_helpers:teardown_steps()).


%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
init_per_group(_Groupname, Config) ->
    Config.

end_per_group(_Groupname, _Config) ->
    ok.


%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(TestCase, Config) ->
    rabbit_ct_helpers:testcase_started(Config, TestCase).

end_per_testcase(TestCase, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, TestCase).


%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================
no_components_tests(Config) ->
    {ok, {{_HTTP, _Code, _}, _Headers, ResBody}} = http_get(Config, "/components"),
    Decoded = jsone:decode(list_to_binary(ResBody), [{object_format, proplist}]),
    [{<<"components">>, []}] = Decoded,
    passed.

all_components_tests(Config) ->
    Channel = rabbit_ct_client_helpers:open_channel(Config),
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, #'exchange.declare'{exchange = ?REQ_EXCHANGE}),
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{}),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, #'queue.bind'{queue = Queue, exchange = ?REQ_EXCHANGE, routing_key = <<"TestComponent1">>}),
    #'queue.bind_ok'{} = amqp_channel:call(Channel, #'queue.bind'{queue = Queue, exchange = ?REQ_EXCHANGE, routing_key = <<"TestComponent2">>}),

    {ok, {{_HTTP, 200, _}, _Headers, ResBody}} = http_get(Config, "/components"),
    Decoded = jsone:decode(list_to_binary(ResBody), [{object_format, proplist}]),
    [{<<"components">>, [
                         [{<<"vhost">>, <<"/">>}, {<<"name">>, <<"TestComponent1">>}],
                         [{<<"vhost">>, <<"/">>}, {<<"name">>, <<"TestComponent2">>}]
                        ]}] = Decoded,
    passed.

http_get(_Config, Path) ->
    httpc:request(get,
                  {"http://localhost:15673/api" ++ Path, []},
                  ?HTTPC_OPTS,
                  []).

