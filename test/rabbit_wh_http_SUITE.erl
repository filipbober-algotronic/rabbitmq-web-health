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
         no_components_tests/1
        ]).

-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").

-define(HTTPC_OPTS, [{version, "HTTP/1.0"}, {autoredirect, false}]).

all() ->
    [
     {group, non_parallel_tests}
    ].

groups() ->
    [
     {non_parallel_tests, [], [
                               no_components_tests
                              ]}

    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    inets:start(),
    Config1 = rabbit_ct_helpers:set_config(Config, [
                                                    {rmq_nodename_suffix, ?MODULE}
                                                   ]),
    rabbit_ct_helpers:run_setup_steps(Config1,
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
    {ok, {{_HTTP, Code, _}, _Headers, ResBody}} = http_get(Config, "/components"),
    [{<<"components">>, []}] = jsone:decode(list_to_binary(ResBody), [{object_format, proplist}]),
    passed.

http_get(_Config, Path) ->
    httpc:request(get,
                  {"http://localhost:15673/api" ++ Path, []},
                  ?HTTPC_OPTS,
                  []).

