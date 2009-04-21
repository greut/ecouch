-module(ecouch_view_SUITE).
-author('Yoan Blanc').
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

all() -> [test_view_creation, test_view_access, test_view_deletion].

test_view_creation()->
    [{userdata, {doc, "Creating a view in an existing database."}}].

test_view_creation(_Config) -> 
    ?line {ok, {obj, [{"ok", true}, {"id", Id}, {"rev", Rev}]}} = ecouch:view_create("ecouch_ct_test", "creation", {obj, [{"all", {obj, [{map,"function(){ emit(null, doc) }"}]}}]}),
    ?line {ok, {obj, [{"_id", Id},
                      {"_rev", Rev},
                      {"language", "javascript"},
                      {"views", {obj, [{"all", {obj, [{"map", _Func}]}}]}}
                     ]}} = ecouch:view_get("ecouch_ct_test", "creation").

test_view_access()->
    [{userdata, {doc, "Accessing an existing view."}}].

test_view_access(_Config)->
    ?line {ok, _JsonObj} = ecouch:view_create("ecouch_ct_test", "access", {obj, [{"all", {obj, [{map,"function(){ emit(null, doc) }"}]}}]}),
    ?line {ok, {obj, [{"total_rows",0},{"rows",[]}]}} = ecouch:view_access("ecouch_ct_test", "access", "all", [{limit, 11}]).

test_view_deletion() ->
    [{userdata, {doc, "Deleting a view in an existing database."}}].

test_view_deletion(_Config) ->
    ?line {ok, {obj, [{"ok", true}, {"id", _Id}, {"rev", Rev}]}} = ecouch:view_create("ecouch_ct_test", "deleteme", {obj, [{"all", {obj, [{map,"function(){ emit(null, doc) }"}]}}]}),
    ?line {ok, {obj, [{"ok", true}, {"id", _Id}, {"rev", _Rev}]}}  = ecouch:view_delete("ecouch_ct_test", "deleteme", Rev).
