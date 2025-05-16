-module(influxdb_SUITE).
-compile(nowarn_export_all).
-compile(export_all).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [
    {group, test_influxdb_client}
].

groups() -> [{test_influxdb_client, [sequence], [test_influxdb_connection,test_influxdb_logging]}].

init_per_suite(Config) ->
    application:ensure_all_started(influxdb),
    Config.

init_per_group(_, Config) ->
    InfluxdbConfig = influxdb_config:new(#{host => "localhost", port => 18086, username => "root", password => "root"}),
    [{influxdb_config, InfluxdbConfig} | Config].

end_per_group(_, Config) ->
    Config.

end_per_suite(_Config) ->
    application:stop(influxdb),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

test_influxdb_connection(Config) ->
    %% Test database creation functionality
    InfluxdbConfig = ?config(influxdb_config, Config),
    ok = influxdb:query(InfluxdbConfig, "create database influxd_test"),
    %% Verify if the database was created successfully
    {ok, DatabaseResult} = influxdb:query(InfluxdbConfig,  "show databases"),
    ?assertEqual(lists:member({<<"influxd_test">>}, maps:get(rows, hd(hd(DatabaseResult)))), true).

test_influxdb_logging(Config) ->
    %% Test logging functionality
    InfluxdbConfig = ?config(influxdb_config, Config),
    ok = influxdb:write(InfluxdbConfig#{database => "influxd_test"}, [{"cpu_load_short",
    #{"region" => "af-west", "host" => "server02"},
    #{"value" => 0.67}}]),
    %% Check if the log entry was created
    {ok, Result} = influxdb:query(InfluxdbConfig#{database => "influxd_test"}, "select * from cpu_load_short"),
    ?assertMatch({_, <<"server02">>, <<"af-west">>, 0.67}, hd(maps:get(rows, hd(hd(Result))))).
