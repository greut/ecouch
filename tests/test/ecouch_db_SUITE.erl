-module(ecouch_db_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-import(ct).
-import(test_helper).
-import(ecouch).
-import(lists).
% Let's error out if our tests take over a minute to complete. This can be reconfigured
% on a per testcase basis in init_per_testcase.
suite() -> [{timetrap, {minutes, 1}}].


init_per_suite(Config) ->
    ok = application:start(inets),
    ok = application:start(ecouch),
    Config.
    
end_per_suite(_Config) ->
    test_helper:delete("ecouch_ct_test"),
    application:stop(ecouch),
    application:stop(inets),
    ok.

init_per_testcase(_TestCase, Config) ->
    test_helper:recreate_db(),
    Config.
    
end_per_testcase(_TestCase, Config) ->
    Config.

all() -> [test_database_creation, test_database_deletion, test_database_listing].

test_database_creation() ->
    [{userdata,
        {doc, "Creating a new database."}}].
test_database_creation(_Config) ->
    % Make sure the database doesn't already exist
    {ok, _} = test_helper:delete("ecouch_db_example_test"),
    
    % Now test.
    ?line {ok, {obj, [{"ok", true}]}} = ecouch:db_create("ecouch_db_example_test"),
    ?line {ok, {{_,200,_}, _,_}} = http:request(delete, {"http://127.0.0.1:5984/ecouch_db_example_test", []}, [], []),
    ok.

test_database_deletion() ->
    [{userdata, {doc, "Deleting an existing database."}}].
test_database_deletion(_Config) ->
    {ok,{{_, 200, _}, _, _}} = test_helper:get("ecouch_ct_test"),
    ?line {ok, {obj, [{"ok", true}]}} = ecouch:db_delete("ecouch_ct_test"),
    {ok,{{_, 404, _}, _, _}} = test_helper:get("ecouch_ct_test"),
    ok.

test_database_listing() ->
    [{userdata, {doc, "Listing the databases."}}].

test_database_listing(_Config) ->
    ?line {ok, DbList} = ecouch:db_list(),
    ?line lists:member(<<"ecouch_ct_test">>, [DbList]),
    ok.