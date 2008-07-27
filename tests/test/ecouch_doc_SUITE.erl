-module(ecouch_doc_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-import(ct).
-import(test_helper).
-import(ecouch).

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
    
all() -> [test_doc_creation_without_id, test_doc_creation_with_id, test_doc_get, test_doc_get_all].

test_doc_creation_without_id() ->
    [{userdata, {doc, "Creating a document in an existing database."}}].
    
test_doc_creation_without_id(_Config) ->
    ?line {ok, {obj, [{"ok", true}, {"id", BinId}, {"rev", BinRev}]}} = ecouch:doc_create("ecouch_ct_test", {obj, [{"foo", 1}]}),

    ?line {ok, {{_, 200, _}, _, Body}} = test_helper:get("ecouch_ct_test", binary_to_list(BinId)),
    
    ?line {ok, {obj, [{"_id", BinId}, {"_rev", BinRev}, {"foo", 1}]}, []} = rfc4627:decode(Body),
    ok.

test_doc_creation_with_id() ->
    [{userdata, {doc, "Creating a document in an existing database with an id given"}}].
    
test_doc_creation_with_id(_Config) ->
    ?line {ok, {obj, [{"ok", true}, {"id", BinId}, {"rev", BinRev}]}} = 
        ecouch:doc_create("ecouch_ct_test", "foo_has_one", {obj, [{"foo", 1}]}),
        
    ?line "foo_has_one" = binary_to_list(BinId), % Make sure the id was set correctly
    
    ?line {ok, {{_, 200, _}, _, Body}} = test_helper:get("ecouch_ct_test", "foo_has_one"),
    ?line {ok, {obj, [{"_id", BinId}, {"_rev", BinRev}, {"foo", 1}]}, []} = rfc4627:decode(Body),
    ok.
    
test_doc_get() ->
    [{userdata, {doc, "Grabbing a document from an existing database"}}].
    
test_doc_get(_Config) ->
    Body = {obj, [{"foo", 2}]},
    JsonBody = rfc4627:encode(Body),
    ?line {ok, {{_, 201, _}, _, _}} = test_helper:put("ecouch_ct_test", "foo_has_two", JsonBody),
    ?line {ok, {obj, [{"_id", <<"foo_has_two">>}, {"_rev", _}, {"foo", 2}]}} = ecouch:doc_get("ecouch_ct_test", "foo_has_two").
    
test_doc_get_all() ->
    [{userdata, {doc, "Get all of the documents from an existing database."}}].
test_doc_get_all(_Config) ->
    JsonBody1 = rfc4627:encode({obj, [{"foo", 1}]}),
    JsonBody2 = rfc4627:encode({obj, [{"foo", 2}]}),
    
    test_helper:get("right_here_fool"),
    ?line {ok, {{_, 201, _}, _, _}} = test_helper:put("ecouch_ct_test", "foo_has_one", JsonBody1),
    ?line {ok, {{_, 201, _}, _, _}} = test_helper:put("ecouch_ct_test", "foo_has_two", JsonBody2),
    
    ?line {ok, {obj, [{"total_rows", 2}, {"offset", 0}, {"rows", Docs}]}} = ecouch:doc_get_all("ecouch_ct_test"),
    Keys = lists:map(fun({obj, DocProps}) -> proplists:get_value("key", DocProps) end, Docs),
    ?line true = lists:member(<<"foo_has_one">>, Keys),
    ?line true = lists:member(<<"foo_has_two">>, Keys).
    