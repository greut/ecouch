-module(ecouch_client_SUITE).
-author('Yoan Blanc').

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-import(ct).
-import(test_helper).
-import(ec_client).
-import(lists).
% Let's error out if our tests take over a minute to complete. This can be reconfigured
% on a per testcase basis in init_per_testcase.
suite() -> [{timetrap, {minutes, 1}}].

init_per_suite(Config) ->
    Config.
    
end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.
    
end_per_testcase(_TestCase, Config) ->
    Config.

all() -> [test_url_encode, test_query_string].

test_url_encode(_Config) ->
    ?line "%20" = ec_client:url_encode(" "),
    ?line "%27" = ec_client:url_encode("'"),
    ?line "%22" = ec_client:url_encode("\""),
    ?line "%26" = ec_client:url_encode("&"),
    ?line "%3D" = ec_client:url_encode("="),
    ?line "%5C" = ec_client:url_encode("\\"),
    ok.

test_query_string(_Config) ->
    ?line [] = ec_client:query_string([]),
    ?line "?key=value" = ec_client:query_string([{key, value}]),
    ?line "?key=value" = ec_client:query_string([{"key", "value"}]),
    ?line "?key=value&key=value2" = ec_client:query_string([{key, value}, {key, value2}]),
    ?line "?key=2" = ec_client:query_string(key, 2),
    ?line "?key=hello%20world" = ec_client:query_string(key, "hello world"),
    ?line "?key=%22apps%22" = ec_client:query_string(key, "\"apps\""),
    ok.
